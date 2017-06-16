name := "tppasRandomEvosList"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "SNAPSHOT"

pipelineStages := Nil

enablePlugins(com.typesafe.sbt.web.SbtWeb)

com.rayrobdod.possibleEvolutions.MyBuild.mySettings




(unmanagedSourceDirectories in Compile) += baseDirectory.value / "project"
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.5"
libraryDependencies += ("com.opencsv" % "opencsv" % "3.4")
enablePlugins(JmhPlugin)
(unmanagedSources in Compile) := (unmanagedSources in Compile).value.filter{_.getName != "build.scala"}
