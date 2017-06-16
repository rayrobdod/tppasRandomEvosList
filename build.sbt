name := "tppasRandomEvosList"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "SNAPSHOT"

pipelineStages := Nil

enablePlugins(com.typesafe.sbt.web.SbtWeb)

com.rayrobdod.possibleEvolutions.MyBuild.mySettings




(unmanagedSourceDirectories in Compile) += baseDirectory.value / "project"
libraryDependencies += ("com.rayrobdod" %% "anti-xml" % "0.7-SNAPSHOT-20150909")
libraryDependencies += ("com.opencsv" % "opencsv" % "3.4")
enablePlugins(JmhPlugin)
(unmanagedSources in Compile) := (unmanagedSources in Compile).value.filter{_.getName != "build.scala"}
