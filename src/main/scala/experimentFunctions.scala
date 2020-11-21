import java.io._

import com.opencsv.CSVWriter
import scalismo.common.{Field, NearestNeighborInterpolator, RealSpace, UnstructuredPointsDomain}
import scalismo.geometry.{EuclideanVector, Point, _3D}
import scalismo.io.MeshIO
import scalismo.kernels.{DiagonalKernel, GaussianKernel, PDKernel}
import scalismo.mesh.{MeshMetrics, TriangleMesh, TriangleMesh3D}
import scalismo.numerics.FixedPointsUniformMeshSampler3D
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel}
import scalismo.statisticalmodel.dataset.{DataCollection, ModelMetrics}
import scalismo.utils.Random

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer



object experimentFunctions {
  /*
  * This script houses the experiment functions
  * 12 October 2020
  * Fabio Fehr
  * */

  implicit val rng = scalismo.utils.Random(42)

  // This function scales any mesh by the scale amount
  def scaleMesh(mesh:TriangleMesh[_3D], scale: Int) : TriangleMesh[_3D] = {
    // Scales it
    val scaledPoints = mesh.pointSet.pointIds.map(id =>
      (mesh.pointSet.point(id).toVector * scale).toPoint
    ).toIndexedSeq
    // Returns a new scaled mesh
    val newMesh: TriangleMesh[_3D] = new TriangleMesh3D(UnstructuredPointsDomain[_3D](scaledPoints), mesh.triangulation)
    newMesh
  }

  // This function lists directories
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def writeCsvFile(
                    fileName: String,
                    header: List[String],
                    rows: List[List[String]]
                  ): Try[Unit] =
    Try(new CSVWriter(new BufferedWriter(new FileWriter(fileName)))).flatMap((csvWriter: CSVWriter) =>
      Try{
        csvWriter.writeAll(
          (header +: rows).map(_.toArray).asJava
        )
        csvWriter.close()
      } match {
        case f @ Failure(_) =>
          // Always return the original failure.  In production code we might
          // define a new exception which wraps both exceptions in the case
          // they both fail, but that is omitted here.
          Try(csvWriter.close()).recoverWith{
            case _ => f
          }
        case success =>
          success
      }
    )

  /*The trainingFunction
  * Takes in one of modelName = "PCA", "Gauss", "MultiScaleGauss", "PCAGauss", "MultiScaleGaussPCA"
  * see modelSelection function
  *
  * Takes in one of trainingScheme = "posesCV", "peopleCV", "LOOCV" // for faust
  * Takes in trainingScheme = "posesCV", "peopleCV", "LOOCV" // for femurs
  * Reference = Reference mesh
  * meshes = all meshes
  * folds = 10 or 100 for cross validation for FAUST
  * folds = 50 for cross validation for Femurs
  * */

  def trainingFunction(modelName: String,
                       dataName: String,
                       trainingScheme: String,
                       reference: TriangleMesh[_3D],
                       meshes:IndexedSeq[TriangleMesh[_3D]],
                       fold: Int) = {

    // mutable appendable lists for storing metrics
    var timeStore = new ListBuffer[String]()
    var foldStore = new ListBuffer[String]()
    var modelGeneralizationStore = new ListBuffer[String]()
    var modelGeneralizationStorePC1 = new ListBuffer[String]()
    var modelGeneralizationHausdorffStore = new ListBuffer[String]()
    var modelGeneralizationHausdorffStorePC1 = new ListBuffer[String]()
    var modelSpecificityStore = new ListBuffer[String]()
    var modelSpecificityStorePC1 = new ListBuffer[String]()

    val foldSize = meshes.size / fold

    // formatting
    val fmt = new java.text.DecimalFormat("#,##0.##############")

    // cross validation section //////////////////////////////////////////////////////////////////////////////////////
    (0 until fold).map(k => {

      println("current fold: %d".format(k))
      foldStore += fmt.format(k+1).toString

      // testMesh set according to training scheme
      val testMeshes:IndexedSeq[TriangleMesh[_3D]] = if(trainingScheme == "posesCV"){
        // 10 folds each pose is seen as a test set
        val testMeshes :IndexedSeq[TriangleMesh[_3D]] = (0 until 10).map(i => {
          IndexedSeq(meshes(i*10+k))
        }).toIndexedSeq.flatten
        testMeshes
      } else
        meshes.slice(k*foldSize,k*foldSize+foldSize)

      // Testing data collection
      val testingdc = DataCollection.fromMeshSequence(reference, testMeshes)._1.get

      val trainMeshes = meshes.diff(testMeshes)

      // Visualise the training set
      //    val ui = ScalismoUI()
      //    val testGroup = ui.createGroup("TestingData")
      //    (0 until testMeshes.size).map{e : Int => ui.show(testGroup, testMeshes(e), "shape%d".format(e))}

      // build model on the train
      val t1 = System.nanoTime
      val model = modelSelection(modelName,reference,trainMeshes)
      val durationSec = (System.nanoTime - t1) / 1e9d
      println("The time taken for this model "+ durationSec)
      timeStore += fmt.format(durationSec).toString // store the time

      // Create a 1 PC model
      val numBasisFunctions = 1
      val modelGP = model.gp.interpolateNearestNeighbor
      val newGP = GaussianProcess(modelGP.mean,modelGP.cov)
      val sampler = FixedPointsUniformMeshSampler3D(reference, numBasisFunctions)
      val LowRankGPPC1 = LowRankGaussianProcess.approximateGPNystrom(newGP, sampler, numBasisFunctions)
      val modelPC1 = StatisticalMeshModel(reference, LowRankGPPC1)

      // Gather metrics //////////////////////////////////////////////////////////////////////////////////////////////
      // Generalisation
      val modelGeneralization = generalization(model,testingdc).get
      modelGeneralizationStore += fmt.format(modelGeneralization).toString // saving
      println("The models average generalisation is the following: " + modelGeneralization)

      // Generalisation PC1
      val modelGeneralizationPC1 = generalization(modelPC1,testingdc).get
      modelGeneralizationStorePC1 += fmt.format(modelGeneralizationPC1).toString // saving
      println("The PC1 models average generalisation is the following: " + modelGeneralizationPC1)

      val modelGeneralizationHausdorff = generalizationHausdorff(model,testingdc).get
      modelGeneralizationHausdorffStore += fmt.format(modelGeneralizationHausdorff).toString
      println("The models Hausdorff generalisation is the following: " + modelGeneralizationHausdorff)

      val modelGeneralizationHausdorffPC1 = generalizationHausdorff(modelPC1,testingdc).get
      modelGeneralizationHausdorffStorePC1 += fmt.format(modelGeneralizationHausdorffPC1).toString
      println("The PC1 models Hausdorff generalisation is the following: " + modelGeneralizationHausdorffPC1)

      // Specificity
      val modelSpecificity = specificity(model,testMeshes,20) // This proceedure is sampled n times
      modelSpecificityStore += fmt.format(modelSpecificity).toString
      println("The models specificity is the following: " + modelSpecificity)

      val modelSpecificityPC1 = specificity(modelPC1,testMeshes,20) // This proceedure is sampled n times
      modelSpecificityStorePC1 += fmt.format(modelSpecificityPC1).toString
      println("The PC1 models specificity is the following: " + modelSpecificityPC1)

    })

    // Save to CSV ///////////////////////////////////////////////////////////////////////////////////////////////////
    val header: List[String] = List("Fold","Time",
      "Avg.Generalisation.PC1","Haus.Generalisation.PC1","Specificity.PC1",
      "Avg.Generalisation","Haus.Generalisation","Specificity")

    val rows: List[List[String]] =
      foldStore.toList.zip(
        timeStore.toList.zip(
          modelGeneralizationStorePC1.toList.zip(
            modelGeneralizationHausdorffStorePC1.toList.zip(
              modelSpecificityStorePC1.toList.zip(
                modelGeneralizationStore.toList.zip(
                  modelGeneralizationHausdorffStore.toList.zip(
                    modelSpecificityStore.toList
                  ))))))).foldLeft(List.empty[List[String]]){
        case (acc, (a, (b, (c, (d, (e, (f, (g,h)))))))) => List(a, b, c, d, e, f, g, h) +: acc
      }.reverse

    writeCsvFile("results/"+dataName+modelName+trainingScheme+".csv", header, rows)
  }

  /*The ModelSelection
 Basically a big switchboard for selecting your model of choice.
 Has options such as PCA, kernels such as Gauss, MultiScale and combinations of both
 To add new kernels just add a new case and return a statistalMeshModel
 */

  def modelSelection(modelType: String,
                     reference:TriangleMesh[_3D],
                     trainMeshes: IndexedSeq[TriangleMesh[_3D]],
                     s : Double = 100, l : Double = 100) : StatisticalMeshModel = {

    modelType match {

      case "PCA" => {
        val trainingdc = DataCollection.fromMeshSequence(reference, trainMeshes)._1.get
        StatisticalMeshModel.createUsingPCA(trainingdc).get
      }
      case "Gauss" => {
        val zeroMean = Field(RealSpace[_3D], (pt:Point[_3D]) => EuclideanVector(0,0,0))
        // The covariance function: (Kernel)
        val scalarValuedGaussianKernel : PDKernel[_3D]= GaussianKernel(sigma = l)*s
        // now we make it matrix values
        val matrixValuedGaussianKernel = DiagonalKernel(scalarValuedGaussianKernel, 3)
        // now we can build the GP
        val gp = GaussianProcess(zeroMean, matrixValuedGaussianKernel)
        val lowRankGP = LowRankGaussianProcess.approximateGPCholesky(
          reference.pointSet,
          gp,
          relativeTolerance = 0.01,
          interpolator = NearestNeighborInterpolator()
        )
        StatisticalMeshModel(reference, lowRankGP)
      }
      case "MultiScaleGauss" => {
        val zeroMean = Field(RealSpace[_3D], (pt:Point[_3D]) => EuclideanVector(0,0,0))

        // AS PER LUTHI PAPER
        //
        // Changing s seems to just change the size of deformations which should be small in the joints
        // Changing l the standard devation we want non smooth deformations in the joints but not jaggard
        val GaussScalarValuedKernel1 = GaussianKernel[_3D](l/1) * (s/1)
        val GaussScalarValuedKernel2 = GaussianKernel[_3D](l/2) * (s/2)
        val GaussScalarValuedKernel3 = GaussianKernel[_3D](l/3) * (s/3)
        val sumMultiscaleGaussScalarValuedKernel = GaussScalarValuedKernel1 + GaussScalarValuedKernel2 + GaussScalarValuedKernel3
        val multiScaleGaussmatrixValuedKernel = DiagonalKernel(sumMultiscaleGaussScalarValuedKernel,3)
        val gp = GaussianProcess(zeroMean, multiScaleGaussmatrixValuedKernel)
        val lowRankGP = LowRankGaussianProcess.approximateGPCholesky(
          reference.pointSet,
          gp,
          relativeTolerance = 0.01,
          interpolator = NearestNeighborInterpolator()
        )
        StatisticalMeshModel(reference, lowRankGP)
      }
      case "PCAGauss" => {
        // Get PCA model
        val PCAmodel = modelSelection("PCA",reference,trainMeshes,s,l)
        val gpSSM = PCAmodel.gp.interpolate(NearestNeighborInterpolator())
        val covSSM = gpSSM.cov
        // Augment with Gauss
        val scalarValuedGaussianKernel : PDKernel[_3D]= GaussianKernel(sigma = l)*s
        val matrixValuedGaussianKernel = DiagonalKernel(scalarValuedGaussianKernel, 3)
        val augmentedCov = covSSM + matrixValuedGaussianKernel

        val augmentedGP = GaussianProcess(gpSSM.mean, augmentedCov)

        val lowRankAugmentedGP = LowRankGaussianProcess.approximateGPCholesky(
          reference.pointSet,
          augmentedGP,
          relativeTolerance = 0.01,
          interpolator = NearestNeighborInterpolator(),
        )
        // now we have a plain PCA model and then a augmented PCA model
        StatisticalMeshModel(reference, lowRankAugmentedGP)
      }
      case "PCAMultiScaleGauss" => {
        // Get PCA model
        val PCAmodel = modelSelection("PCA",reference,trainMeshes,s,l)
        val gpSSM = PCAmodel.gp.interpolate(NearestNeighborInterpolator())
        val covSSM = gpSSM.cov
        // Augment with MultiScaleGauss
        val GaussScalarValuedKernel1 = GaussianKernel[_3D](l/1) * (s/1)
        val GaussScalarValuedKernel2 = GaussianKernel[_3D](l/2) * (s/2)
        val GaussScalarValuedKernel3 = GaussianKernel[_3D](l/3) * (s/3)
        val sumMultiscaleGaussScalarValuedKernel = GaussScalarValuedKernel1 + GaussScalarValuedKernel2 + GaussScalarValuedKernel3
        val multiScaleGaussmatrixValuedKernel = DiagonalKernel(sumMultiscaleGaussScalarValuedKernel,3)

        val augmentedCov = covSSM + multiScaleGaussmatrixValuedKernel

        val augmentedGP = GaussianProcess(gpSSM.mean, augmentedCov)

        val lowRankAugmentedGP = LowRankGaussianProcess.approximateGPCholesky(
          reference.pointSet,
          augmentedGP,
          relativeTolerance = 0.01,
          interpolator = NearestNeighborInterpolator(),
        )
        // now we have a plain PCA model and then a augmented PCA model
        StatisticalMeshModel(reference, lowRankAugmentedGP)
      }
    }
  }

  def getMetrics(model: StatisticalMeshModel,
                 durationSec: Double,
                 reference: TriangleMesh[_3D],
                 testdata: Seq[TriangleMesh[_3D]],
                 experimentName: String,
                 modelName: String,
                 dataName: String
                )= {
    /*This function takes in a model and some labels and saves the output in CSV*/

    // create a DataCollection
    val dc = DataCollection.fromMeshSequence(reference, testdata)._1.get

    // Generalisation
    val modelGeneralization = ModelMetrics.generalization(model,dc).get
    val modelGeneralizationHausdorff = generalizationHausdorff(model,dc).get
    println("The models average generalisation is the following: " + modelGeneralization)
    println("The models Hausdorff generalisation is the following: " + modelGeneralizationHausdorff)

    // Specificity
    val modelSpecificity = ModelMetrics.specificity(model,testdata,20) // This proceedure is sampled n times
    println("The models specificity is the following: " + modelSpecificity)

    // Save to CSV
    val fmt = new java.text.DecimalFormat("#,##0.##############")
    val header: List[String] = List("Experiment", "Model", "Data", "Time","Avg.Generalisation","Haus.Generalisation","Specificity")
    val rows: List[List[String]] = List(List(experimentName,
      modelName,
      dataName,
      fmt.format(durationSec).toString,
      fmt.format(modelGeneralization).toString,
      fmt.format(modelGeneralizationHausdorff).toString,
      fmt.format(modelSpecificity).toString))

    writeCsvFile("results/"+dataName+modelName+experimentName+".csv", header, rows)

  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Distance metrics //////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def correspondenceAverageDistance(m1: TriangleMesh[_3D], m2: TriangleMesh[_3D]): Double = {
    require(m1.pointSet.numberOfPoints == m2.pointSet.numberOfPoints)

    val dists = (m1.pointSet.points.toIndexedSeq zip m2.pointSet.points.toIndexedSeq).map {
      case (m1P, m2P) => (m1P - m2P).norm
    }
    dists.sum / m1.pointSet.numberOfPoints
  }

  def correspondenceHausdorffDistance(m1: TriangleMesh[_3D], m2: TriangleMesh[_3D]): Double = {

    require(m1.pointSet.numberOfPoints == m2.pointSet.numberOfPoints)

    val dists = (m1.pointSet.points.toIndexedSeq zip m2.pointSet.points.toIndexedSeq).map {
      case (m1P, m2P) => (m1P - m2P).norm
    }
    dists.max
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Generalisation //////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //Generalisation consider Average distance and Hausenhorff
  // In case you which to evaluate the generalization capability as a function of the number of model parameters,
  //truncate (function that reduces rank) the model and evaluate the generalization with a decreasing number of parameters.
  // notice also that generalisation averages all the errors. In Luthis paper we dont haver but rather get all the point
  // errors and then plot in a box plot.

  def generalization(pcaModel: StatisticalMeshModel, dc: DataCollection): Try[Double] = {

    if (pcaModel.referenceMesh == dc.reference) Success {
      dc.dataItems.par.map { item =>
        val mesh = dc.reference.transform(item.transformation)
        val projection = pcaModel.project(mesh)
        correspondenceAverageDistance(projection, mesh)
      }.sum / dc.size.toDouble
    } else Failure(new Exception("pca model and test data collection must have the same reference"))
  }

  def generalizationHausdorff(pcaModel: StatisticalMeshModel, dc: DataCollection): Try[Double] = {

    if (pcaModel.referenceMesh == dc.reference) Success {
      dc.dataItems.par.map { item =>
        val mesh = dc.reference.transform(item.transformation)
        MeshIO.writeMesh(mesh, new File("currentdMesh1.stl"))
        val projection = pcaModel.project(mesh)
        MeshIO.writeMesh(projection, new File("projectedMesh1.stl"))
        correspondenceHausdorffDistance(projection, mesh)
      }.sum / dc.size.toDouble
    } else Failure(new Exception("pca model and test data collection must have the same reference"))
  }


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Specificity /////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def specificity(pcaModel: StatisticalMeshModel, data: Iterable[TriangleMesh[_3D]], nbSamples: Int)(
    implicit rng: Random
  ): Double = {

    (0 until nbSamples).par.map { _ =>
      val sample = pcaModel.sample
      data.map { m =>
        correspondenceAverageDistance(m, sample)
      }.min
    }.sum * (1.0 / nbSamples)
  }

  //  val modelSpecificity = ModelMetrics.specificity(model,meshes,100) // This proceedure is sampled 20 times
  //  println("The models specificity is the following: " + modelSpecificity)
  //  The models specificity is the following: 6.619906408453135

  // Specificity consider 1st PC and the full model


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Compactness /////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //  // Compactness is known to just be the sum of the eigenvalues
  //  def sumVarianceFirstNFunctions(ssm : StatisticalMeshModel, n : Int) : Double = {
  //    ssm.gp.klBasis
  //      .take(n)
  //      .map(ithBasis => ithBasis.eigenvalue)
  //      .sum
  //  }
  //
  //  // 100 is assuming all basis functions
  //  val modelCompactness = sumVarianceFirstNFunctions(model, 100)
  //  println("The models compactness is the following: " + modelCompactness)

}