object femur_GPMM extends App {
  // 12 October 2020
  // Fabio Fehr
  // This experiment runs all for femurs GPMMs

  import scalismo.geometry._
  import scalismo.mesh._
  import scalismo.io.{MeshIO}
  import experimentFunctions.{trainingFunction,getListOfFiles}
  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)

  // Load the data  //////////////////////////////////////////////////////////////////////////////////////////////////
  val meshFiles = getListOfFiles("meshes/femurs/processedFemurs/")
  val extractor = "processedFemur([\\d]+).stl$".r

  // Sorts the files into a numbered order
  val sortedMeshFiles = meshFiles.sortWith {(l, r) =>
    val extractor(lFileNumber) = l.getName
    val extractor(rFileNumber) = r.getName

    lFileNumber.toInt < rFileNumber.toInt
  }
  val meshes = sortedMeshFiles.map(meshFile => {
    val mesh = MeshIO.readMesh(meshFile).get
    mesh
  }).toIndexedSeq

  // Set reference
  val reference = meshes.head // the first dataset dataset(0)
  // Set model parameters  ////////////////////////////////////////////////////////////////////////////////////////////

  // If we want to train for poses set:
  // trainingScheme = "posesCV" AND fold = 10
  // If we want to train for people set:
  // trainingScheme = "peopleCV" AND fold = 10
  // If we want to train for poses set:
  // trainingScheme = "LOOCV" AND fold = 100

  // Labels
  val modelName = List("PCA", "Gauss", "MultiScaleGauss", "PCAGauss", "PCAMultiScaleGauss") // "PCA", "Gauss", "MultiScaleGauss", "PCAGauss", "PCAMultiScaleGauss"
  val dataName = "Femur"
  val trainingScheme = "LOOCV" // "posesCV" "peopleCV" "LOOCV"
  val fold =  meshes.size

  (0 until modelName.size).map(i => {
    println("Currently running %s".format(trainingScheme))
    trainingFunction(modelName(i),dataName,trainingScheme,reference,meshes,fold)
  })

  println("fin")
}