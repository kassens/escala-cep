import sbt._
import com.github.olim7t.sbtscalariform._

class CEPProject(info: ProjectInfo) extends DefaultProject(info) with ScalariformPlugin {
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.2"

  override def formatBeforeCompiling = false
}
