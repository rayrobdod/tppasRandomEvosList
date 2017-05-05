name := "tppasRandomEvosList"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "SNAPSHOT"

pipelineStages := Nil

enablePlugins(com.typesafe.sbt.web.SbtWeb)
enablePlugins(net.virtualvoid.optimizer.SbtOptimizerPlugin)

com.rayrobdod.possibleEvolutions.MyBuild.mySettings
