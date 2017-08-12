
lazy val shared = crossProject.crossType(SharedCrossType)
	.settings(name := "tppRandomEvos")
	.settings(libraryDependencies ++= Seq(
		  "com.lihaoyi" %% "scalatags" % "0.6.5"
		, "com.opencsv" % "opencsv" % "3.4"
	))
	.settings(mySettings:_*)

// Needed, so sbt finds the projects
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

lazy val website = project
	.enablePlugins(com.typesafe.sbt.web.SbtWeb)
	.settings(name := "tppRandomEvos-website")
	.settings(pipelineStages := Nil)
	.settings(mySettings:_*)
	.settings(
		TaskKey[Seq[File]]("generateHtmlFiles") in Assets := {
			val input = new File("""C:\Users\Raymond\Documents\Programming\HTML-JS\RandomizedEvolutions\shared\data""")
			val target = (resourceManaged in Assets).value
			
			val cp:Seq[java.net.URL] = (fullClasspath in Compile in compiler).value.files.map{_.toURI.toURL}
			val loader = new java.net.URLClassLoader(cp.toArray, this.getClass.getClassLoader)
			val contextClazz = loader.loadClass("com.rayrobdod.possibleEvolutions.Compiler$Context")
			val contextConstructor = contextClazz.getConstructor(loader.loadClass("java.io.File"), loader.loadClass("java.io.File"))
			val contextInstance:java.lang.Object = contextConstructor.newInstance(input, target).asInstanceOf[Object]
			val compilerClazz = loader.loadClass("com.rayrobdod.possibleEvolutions.Compiler")
			val compilerMethod = compilerClazz.getMethod("apply", contextClazz)
			val result = compilerMethod.invoke(null, contextInstance)
			
			result.getClass.getMethod("files").invoke(result).asInstanceOf[Array[_]].toSeq.map{_.asInstanceOf[File]}
		},
		managedResourceDirectories in Assets += (resourceManaged in Assets).value,
		resourceGenerators in Assets += (TaskKey[Seq[File]]("generateHtmlFiles") in Assets).taskValue
	)

lazy val mySettings = Seq(
	organization := "com.rayrobdod",
	organizationHomepage := Some(new URL("http://rayrobdod.name/")),
	version := "SNAPSHOT",
	javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked"),
	scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

name := "aggregate"
