import sbt._

class CEPProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
}