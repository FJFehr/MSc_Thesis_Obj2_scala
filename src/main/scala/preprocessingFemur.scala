object preprocessingFemur extends App {

  /* Using the code from Tutorial 12 I have successfully aligned and put the femurs in correspondence
  * The femurs were decimate the femurs into a lower dimension with fewer verticies
  * The femurs were scaled to all be between -1:1 thus dividing by 1/250
  * Fabio Fehr
  * 18 October 2020
  * */

  import scalismo.geometry._
  import scalismo.common._
  import scalismo.ui.api._
  import scalismo.mesh._
  import scalismo.registration._
  import scalismo.io.{MeshIO,StatisticalModelIO,LandmarkIO}
  import scalismo.numerics._
  import scalismo.kernels._
  import scalismo.statisticalmodel._
  import breeze.linalg.{DenseVector}
  import java.util.Arrays

  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)
  val ui = ScalismoUI()

  // Set paths
  val meshPath = "meshes/femurs/rawFemurs/"
  val meshType = ".stl"
  val landmarkPath = "meshes/femurs/rawLandmarks/"
  val landmarkType = ".json"
  val outputPath = "meshes/femurs/processedFemurs/"
  val outputName = "processedFemur"

  // This function scales any mesh by the scale amount
  def scaleMesh(mesh:TriangleMesh[_3D], scale: Double) : TriangleMesh[_3D] = {
    // Scales it
    val scaledPoints = mesh.pointSet.pointIds.map(id =>
      (mesh.pointSet.point(id).toVector * scale).toPoint
    ).toIndexedSeq
    // Returns a new scaled mesh
    val newMesh: TriangleMesh[_3D] = new TriangleMesh3D(UnstructuredPointsDomain[_3D](scaledPoints), mesh.triangulation)
    newMesh
  }

  // Load the meshes
  println("Loading meshes ...")
  val meshFiles = new java.io.File("%s".format(meshPath)).listFiles
  val Extractor = "([\\d]+)\\.stl".r
  val meshFilesSorted = meshFiles.map(_.getName).sortBy{case Extractor(n) => n.toInt } // Sorts them
  val meshes = meshFilesSorted.map(meshFile => {
    val mesh = MeshIO.readMesh(new java.io.File("%s".format(meshPath)+meshFile)).get
    mesh
  })

  // define the reference mesh and load landmarks
  val referenceMesh1 = meshes(0) // original reference with 18197 points
  val referenceMesh = referenceMesh1.operations.decimate(2000) // reference mesh of lower dimension 7001 points
  val modelGroup = ui.createGroup("model")
  val refMeshView = ui.show(modelGroup, referenceMesh, "referenceMesh")
  refMeshView.color = java.awt.Color.YELLOW
  val refLms = LandmarkIO.readLandmarksJson[_3D](new java.io.File("%s0%s".format(landmarkPath,landmarkType))).get
  //ui.show(modelGroup, refLms, "referenceLms")
  MeshIO.writeMesh(scaleMesh(referenceMesh,0.004), new java.io.File("%s%s0%s".format(outputPath,outputName,meshType)))

  // Build GP
  println("Building GP for non-rigid alignment")
  // Build the GP from model
  //  val model = StatisticalModelIO.readStatisticalMeshModel(new java.io.File("models/pcamodel-augmented.h5")).get
  //  val lowRankGP : LowRankGaussianProcess[_3D, EuclideanVector[_3D]] = model.gp.interpolate(NearestNeighborInterpolator())
  val mean = VectorField(RealSpace[_3D], (_ : Point[_3D]) => EuclideanVector.zeros[_3D])
  val kernel = DiagonalKernel[_3D](GaussianKernel(sigma = 70) * 50.0, outputDim = 3)
  val gp = GaussianProcess(mean, kernel)

  // get a low rank approx
  val lowRankGP = LowRankGaussianProcess.approximateGPCholesky(
    referenceMesh.pointSet,
    gp,
    relativeTolerance = 0.05,
    interpolator = NearestNeighborInterpolator()
  )

  // Visualise the effect of non rigid alignment
  val gpView = ui.addTransformation(modelGroup, lowRankGP, "gp")

  // make a class for the parameters
  case class RegistrationParameters(regularizationWeight : Double,
                                    numberOfIterations : Int,
                                    numberOfSampledPoints : Int)
  // Now we define the parameters and run the registration
  val regParameters = RegistrationParameters(regularizationWeight = 1e-5, numberOfIterations = 100, numberOfSampledPoints = 1000)
  // initial GP parameters
  val modelCoefficients = DenseVector.zeros[Double](lowRankGP.rank) // zerovector
  // Group for the target output
  val targetGroup = ui.createGroup("target")

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Rigid Alignment //////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def rigidAlignment(item: Int,
                     targetMesh: TriangleMesh[_3D]) = {

    // Fetch the landmarks for the new mesh
    val newLms = LandmarkIO.readLandmarksJson[_3D](new java.io.File("%s%d%s".format(landmarkPath, item, landmarkType))).get
    //ui.show(modelGroup, newLms, "targetLms")

    // Apply procrustes using the following: NB its from - to. from the new mesh to the reference mesh
    // This learns the alignment transformation from a few corresponding points
    val bestTransform = LandmarkRegistration.rigid3DLandmarkRegistration(newLms, refLms)

    // Now we project the new mesh by this whole transformation
    val newAlignedfemur = targetMesh.transform(bestTransform)

    newAlignedfemur
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Non-Rigid Alignment //////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def doRegistration(
                      lowRankGP : LowRankGaussianProcess[_3D, EuclideanVector[_3D]],
                      referenceMesh : TriangleMesh[_3D],
                      targetMesh : TriangleMesh[_3D],
                      registrationParameters : RegistrationParameters,
                      initialCoefficients : DenseVector[Double],
                      name : String,
                      item : Integer
                    ) =
  {
    val transformationSpace = GaussianProcessTransformationSpace(lowRankGP)
    val fixedImage = referenceMesh.operations.toDistanceImage
    val movingImage = targetMesh.operations.toDistanceImage
    val sampler = FixedPointsUniformMeshSampler3D(
      referenceMesh,
      registrationParameters.numberOfSampledPoints
    )
    val metric = MeanSquaresMetric(
      fixedImage,
      movingImage,
      transformationSpace,
      sampler
    )
    val optimizer = LBFGSOptimizer(registrationParameters.numberOfIterations)
    val regularizer = L2Regularizer(transformationSpace)
    val registration = Registration(
      metric,
      regularizer,
      registrationParameters.regularizationWeight,
      optimizer
    )
    val registrationIterator = registration.iterator(initialCoefficients)
    val visualizingRegistrationIterator = for ((it, itnum) <- registrationIterator.zipWithIndex) yield {
      //println(s"object value in iteration $itnum is ${it.value}") //prints update
      gpView.coefficients = it.parameters
      it
    }
    val registrationResult = visualizingRegistrationIterator.toSeq.last

    // We should now see in the GUI, the ref morph into our target
    val registrationTransformation = transformationSpace.transformForParameters(registrationResult.parameters)

    // Now we project it onto the surface to get correspondence
    val targetMeshOperations = targetMesh.operations
    val projection = (pt : Point[_3D]) => {
      targetMeshOperations.closestPointOnSurface(pt).point
    }

    // Now we can get a mapping for each point
    val finalTransformation = registrationTransformation.andThen(projection)

    val projectedMesh = referenceMesh.transform(finalTransformation)
    val projectedMeshScaled = scaleMesh(projectedMesh,0.004) // same as 1/250 gets into domain -1:1
    // Save
    //println("Final Point length " + targetMesh.pointSet.numberOfPoints)
    MeshIO.writeMesh(projectedMeshScaled, new java.io.File("%s%s%d%s".format(outputPath,name, item,meshType)))
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Registration /////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////

  (1 until meshes.size).map{i : Int =>
    val targetMesh = meshes(i)
    //println("Original Point length " + targetMesh.pointSet.numberOfPoints)
    val targetMeshView = ui.show(targetGroup, targetMesh, "unalignedTargetMesh")
    targetMeshView.color = java.awt.Color.RED

    val alignedTarget = rigidAlignment(i,targetMesh)
    val targetMeshView2 = ui.show(targetGroup, alignedTarget, "alignedTargetMesh")
    targetMeshView2.color = java.awt.Color.BLUE
    println("Aligned mesh "+ i)

    doRegistration(lowRankGP, referenceMesh, alignedTarget, regParameters, modelCoefficients,outputName,i)
    println("Registered mesh "+ i)

    targetMeshView.remove()
    targetMeshView2.remove()
  }

  println("fin")
}

