object faust_GPMM extends App {

  // 12 October 2020
  // Fabio Fehr
  // This script runs all experiments for FAUST GPMMs

  import scalismo.geometry._
  import scalismo.mesh._
  import scalismo.io.{MeshIO}
  import experimentFunctions.{trainingFunction,getListOfFiles}
  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)

  // Load the data  //////////////////////////////////////////////////////////////////////////////////////////////////
  val meshFiles = getListOfFiles("meshes/faust/aligned/")
  val extractor = "aligned([\\d]+).ply$".r

  // Sorts the files into a numbered order
  val sortedMeshFiles = meshFiles.sortWith {(l, r) =>
    val extractor(lFileNumber) = l.getName
    val extractor(rFileNumber) = r.getName

    lFileNumber.toInt < rFileNumber.toInt
  }

  // read in the mesh files
  val meshes :IndexedSeq[TriangleMesh[_3D]] = (0 until sortedMeshFiles.size).map(i => {
    println(sortedMeshFiles(i))
    val mesh = MeshIO.readMesh(sortedMeshFiles(i)).get
    mesh
  }).toIndexedSeq

  // Set reference
  val reference = meshes.head

  // Set model parameters  ////////////////////////////////////////////////////////////////////////////////////////////

  // If we want to train for poses set:
  // trainingScheme = "posesCV" AND fold = 10
  // If we want to train for people set:
  // trainingScheme = "peopleCV" AND fold = 10
  // If we want to train for poses set:
  // trainingScheme = "LOOCV" AND fold = 100

  // Labels
  val modelName = List("PCA", "Gauss", "MultiScaleGauss", "PCAGauss", "PCAMultiScaleGauss")
  val dataName = "FAUST"
  val trainingScheme = List("posesCV", "peopleCV", "LOOCV")
  val fold =  List(10,10,100)

  (0 until modelName.size).map(j => {
    (0 until trainingScheme.size).map(i => {
      println("Currently running %s".format(trainingScheme(i)))
      trainingFunction(modelName(j),dataName,trainingScheme(i),reference,meshes,fold(i))
    })
  })


  println("fin")

}