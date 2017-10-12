
import sbt._

object SharedCrossType extends org.scalajs.sbtplugin.cross.CrossType {
	def projectDir(crossBase: File, projectType: String): File =
		crossBase / ("." + projectType)
	def sharedSrcDir(projectBase:File, conf:String):Option[File] =
		Option(projectBase / ".." / "src" / conf / "scala")
}
