import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import com.rayrobdod.possibleEvolutions.SharedCrossType

lazy val compiledOnMacro = crossProject(JVMPlatform, JSPlatform).crossType(SharedCrossType)
	.settings(name := "tppRandomEvos-compiledTimeMacro")
	.settings(mySettings:_*)
	.settings(libraryDependencies ++= Seq(
		"org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
	))
	.jsSettings(
		libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "0.2.5",
	)
lazy val compiledOnMacroJVM = compiledOnMacro.jvm
lazy val compiledOnMacroJS = compiledOnMacro.js

lazy val shared = crossProject(JVMPlatform, JSPlatform).crossType(SharedCrossType)
	.dependsOn(compiledOnMacro)
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
	.settings(name := "tppRandomEvos-compiler")
	.settings(mySettings:_*)
	.settings(com.rayrobdod.possibleEvolutions.GenPrologue.settings)

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
		(mappings in ghpagesSynchLocal) := com.typesafe.sbt.web.Import.WebKeys.pipeline.value,
	)
	.settings(mySettings:_*)
	.settings(
		TaskKey[Seq[File]]("generateHtmlFiles") in Assets := {
			val genMonPagesValue:java.lang.Boolean = (perMonPages in TaskKey[Seq[File]]("generateHtmlFiles") in Assets).value
			val target = (resourceManaged in Assets).value
			
			val cp:Seq[java.net.URL] = (fullClasspath in Compile in compiler).value.files.map{_.toURI.toURL}
			val loader = new java.net.URLClassLoader(cp.toArray, this.getClass.getClassLoader)
			val contextClazz = loader.loadClass("com.rayrobdod.possibleEvolutions.Compiler$Context")
			val contextConstructor = contextClazz.getConstructor(loader.loadClass("java.lang.Boolean"), loader.loadClass("java.io.File"))
			val contextInstance:java.lang.Object = contextConstructor.newInstance(genMonPagesValue, target).asInstanceOf[Object]
			val compilerClazz = loader.loadClass("com.rayrobdod.possibleEvolutions.Compiler")
			val compilerMethod = compilerClazz.getMethod("apply", contextClazz)
			val result = compilerMethod.invoke(null, contextInstance)
			
			result.getClass.getMethod("files").invoke(result).asInstanceOf[Array[_]].toSeq.map{_.asInstanceOf[File]}
		},
		managedResourceDirectories in Assets += (resourceManaged in Assets).value,
		resourceGenerators in Assets += (TaskKey[Seq[File]]("generateHtmlFiles") in Assets).taskValue
	)
	.settings(
		TaskKey[Seq[File]]("theoreticalPageJs") in Assets := {
			val relativeScriptLocation = "style/theoreticalPage.js"
			val target = (resourceManaged in Assets).value / relativeScriptLocation
			val src = new File((scalaJSLinkedFile in Compile in theoreticalPage).value.path)
			
			sbt.IO.createDirectory(target.getParentFile)
			sbt.IO.copyFile(src, target)
			Seq(target)
		},
		managedResourceDirectories in Assets += (resourceManaged in Assets).value,
		resourceGenerators in Assets += (TaskKey[Seq[File]]("theoreticalPageJs") in Assets).taskValue
	)

lazy val mySettings = Seq(
	organization := "com.rayrobdod",
	organizationHomepage := Some(new URL("http://rayrobdod.name/")),
	version := "SNAPSHOT",
	javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked"),
	scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused-import", "-Ywarn-unused", "-Xlint:_", "-Xfuture", "-Xcheckinit")
)

name := "aggregate"
scalaJSStage in Global := org.scalajs.sbtplugin.Stage.FullOpt
perMonPages in Global := true
