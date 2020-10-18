import java.io.File

import scalismo.geometry.Point
import scalismo.registration.LandmarkRegistration

object alignFAUSTData extends App {

  /* Align all FAUST data
  * Fabio Fehr
  * 27 May 2020
  * Assume that the data is in correspondence and as aligned as it can be for now.
  * */

  import scalismo.common.UnstructuredPointsDomain
  import scalismo.geometry._3D
  import scalismo.io.MeshIO
  import scalismo.mesh.{TriangleMesh, TriangleMesh3D}
  import scalismo.ui.api.ScalismoUI
  import scalismo.registration
  import scalismo.common.PointId
  import scalismo.geometry.Landmark


  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)

//  val ui = ScalismoUI()
//  val faustGroup = ui.createGroup("FAUST")

  // Only use the registrations which we are certain of as landmarks for alignment

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

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Read in the files ///////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Load the data  //////////////////////////////////////////////////////////////////////////////////////////////////
  val meshFiles = getListOfFiles("meshes/faust/")
  val extractor = "tr_reg_([\\d]+).ply$".r

  // Sorts the files into a numbered order
  val sortedMeshFiles = meshFiles.sortWith {(l, r) =>
    val extractor(lFileNumber) = l.getName
    val extractor(rFileNumber) = r.getName

    lFileNumber.toInt < rFileNumber.toInt
  }

  val meshes :IndexedSeq[TriangleMesh[_3D]] = (0 until sortedMeshFiles.size).map(i => {
    println(sortedMeshFiles(i))
    val mesh = MeshIO.readMesh(sortedMeshFiles(i)).get
    scaleMesh(mesh, 1) // Only change the scale to visualise in Scala
  }).toIndexedSeq

  // Set reference
  val reference = meshes.head // the first dataset dataset(0)

  // Alignment:
  // Grab some points on the reference
//  val pointIds = (1200 until 1300 by 1).toSeq ++ (3300 until 3400 by 1).toSeq // torso ++ left foot
  val pointIds = (0 until reference.pointSet.numberOfPoints by 10).toSeq // uniform sample GPA
  val refLandmarks = pointIds.map{id => Landmark(s"L_$id", reference.pointSet.point(PointId(id))) }

  // View alignment:
  //  val ui = ScalismoUI()
  //  val refGroup = ui.createGroup("ref")
  //  val refView2 = ui.show(refGroup, reference, "mesh")
  //  val refView = ui.show(refGroup, refLandmarks, "point")

  // This iterates through each mesh, gets the same landmarks as the ref,uses a rigid transformation
  val alignedMeshes = meshes.map { mesh =>
    val landmarks = pointIds.map{id => Landmark("L_"+id, mesh.pointSet.point(PointId(id)))}
    val rigidTrans = LandmarkRegistration.rigid3DLandmarkRegistration(landmarks, refLandmarks, center = Point(0,0,0))
    mesh.transform(rigidTrans)
  }

  // lets save the new aligned mesh
  (0 until alignedMeshes.size).map(i =>{
    println(("aligned%d.ply".format(i)))
    MeshIO.writeMesh(alignedMeshes(i), new File("meshes/faust/aligned/aligned%d.ply".format(i)))
  })



  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  //  import java.io.File
//  import scala.io.Source
//  def getListOfFiles(dir: String): List[String] = {
//    val file = new File(dir)
//    file.listFiles.filter(_.isFile)
//    .filter(_.getName.startsWith("tr_gt"))
//    .map(_.getPath).toList
//  }
//  val file_list = getListOfFiles("data/groundTruth/")

//  // Reads in a file and returns a list
//  def readFile(filename: String): Seq[String] = {
//    val bufferedSource = Source.fromFile(filename)
//    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
//    bufferedSource.close
//    lines
//  }

//  // Loading multiple GT files
//  val all_gt_vertices = file_list.map(file => readFile(file)).toList
//  println(all_gt_vertices)
//
//
//  // Shows how many we have. A lot more then points
//  val file = readFile("data/groundTruth/tr_gt_000.txt")
//  println(file.length)


  println("fin")
}
