import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import com.rayrobdod.possibleEvolutions.SharedCrossType

val generateHtmlFiles = taskKey[Seq[File]]("")
val theoreticalPageJs = taskKey[Seq[File]]("")
import scala.collection.immutable.{Seq => ISeq}

lazy val shared = crossProject(JVMPlatform, JSPlatform).crossType(SharedCrossType)
	.settings(name := "tppRandomEvos")
	.settings(libraryDependencies ++= Seq(
		  "com.lihaoyi" %%% "scalatags" % "0.7.0"
		, "org.scalatest" %%% "scalatest" % "3.0.8" % "test"
	))
	.settings(mySettings:_*)
lazy val sharedJVM = shared.jvm
lazy val sharedJS = shared.js

lazy val benchmark = project
	.dependsOn(sharedJVM)
	.enablePlugins(JmhPlugin)
	.settings(name := "tppRandomEvos-benchmark")
	.settings(mySettings:_*)

lazy val compiler = project
	.dependsOn(sharedJVM)
	.enablePlugins(GenPrologue)
	.settings(name := "tppRandomEvos-compiler")
	.settings(mySettings:_*)

lazy val theoreticalPage = project
	.enablePlugins(ScalaJSPlugin)
	.dependsOn(sharedJS)
	.settings(name := "tppRandomEvos-theoretical")
	.settings(scalaJSUseMainModuleInitializer := true)
	.settings(mySettings:_*)

val perMonPages = taskKey[Boolean]("Whether to create the rather numerous per-mon html pages")
lazy val website = project
	.enablePlugins(com.typesafe.sbt.web.SbtWeb)
	.enablePlugins(GhpagesPlugin)
	.settings(name := "tppRandomEvos-website")
	.settings(pipelineStages := Nil)
	.settings(
		git.remoteRepo := "https://rayrobdod@github.com/rayrobdod/tppasRandomEvosList",
		(ghpagesSynchLocal / mappings) := com.typesafe.sbt.web.Import.WebKeys.pipeline.value,
	)
	.settings(mySettings:_*)
	.settings(
		Assets / generateHtmlFiles / fileInputs += Glob((compiler / crossTarget).value.toPath.resolve("classes"), RecursiveGlob),
		Assets / generateHtmlFiles / fileInputs += Glob((sharedJVM / crossTarget).value.toPath.resolve("classes"), RecursiveGlob),
		Assets / generateHtmlFiles := {
			def compile(params:(java.lang.Boolean, java.io.File, Seq[HashFileInfo])):ISeq[File] = {
				val (genMonPagesValue, target, _) = params
				val classpath:Seq[java.net.URL] = (compiler / Compile / fullClasspath).value.files.map(_.toURI.toURL)
				val loader = new java.net.URLClassLoader(classpath.toArray, this.getClass.getClassLoader)
				val contextClazz = loader.loadClass("com.rayrobdod.possibleEvolutions.Compiler$Context")
				val contextConstructor = contextClazz.getConstructor(loader.loadClass("java.lang.Boolean"), loader.loadClass("java.io.File"))
				val contextInstance:java.lang.Object = contextConstructor.newInstance(genMonPagesValue, target).asInstanceOf[Object]
				val compilerClazz = loader.loadClass("com.rayrobdod.possibleEvolutions.Compiler")
				val compilerMethod = compilerClazz.getMethod("apply", contextClazz)
				val result = compilerMethod.invoke(null, contextInstance)
				result.getClass.getMethod("files").invoke(result).asInstanceOf[Array[_]].to[ISeq].map(_.asInstanceOf[File])
			}

			val genMonPagesValue:java.lang.Boolean = (Assets / generateHtmlFiles / perMonPages).value
			val cacheFactory = (Assets / generateHtmlFiles / streams).value.cacheStoreFactory
			val classpathHashes = (Assets / generateHtmlFiles).inputFiles.map(x => FileInfo.hash(x.toFile))
			val target = (Assets / resourceManaged).value

			import sbt.util.CacheImplicits._
			val tracker = Tracked.inputChanged[(java.lang.Boolean, java.io.File, Seq[HashFileInfo]), ISeq[File]](cacheFactory.make("input")) {
				case (changed:Boolean, params:(java.lang.Boolean, java.io.File, Seq[HashFileInfo])) =>
				val tracker = Tracked.lastOutput[(java.lang.Boolean, java.io.File, Seq[HashFileInfo]), ISeq[File]](cacheFactory.make("last")) {
					case (_, Some(out)) if !changed => out
					case (in, _) => compile(in)
				}
				tracker(params)
			}
			tracker( (genMonPagesValue, target, classpathHashes) )
		},
		Assets / managedResourceDirectories += (Assets / resourceManaged).value,
		Assets / resourceGenerators += (Assets / generateHtmlFiles).taskValue
	)
	.settings(
		Assets / theoreticalPageJs := {
			val relativeScriptLocation = "style/theoreticalPage.js"
			val target = (Assets / resourceManaged).value / relativeScriptLocation
			val src = new File((theoreticalPage / Compile / scalaJSLinkedFile).value.path)

			sbt.IO.createDirectory(target.getParentFile)
			sbt.IO.copyFile(src, target)
			Seq(target)
		},
		Assets / managedResourceDirectories += (Assets / resourceManaged).value,
		Assets / resourceGenerators += (Assets / theoreticalPageJs).taskValue
	)

lazy val mySettings = Seq(
	organization := "com.rayrobdod",
	organizationHomepage := Some(new URL("http://rayrobdod.name/")),
	version := "SNAPSHOT",
	javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked"),
	scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused-import", "-Ywarn-unused", "-Xlint:_", "-Xfuture", "-Xcheckinit")
)

name := "aggregate"
Global / scalaJSStage := org.scalajs.sbtplugin.Stage.FullOpt
Global / perMonPages := true
