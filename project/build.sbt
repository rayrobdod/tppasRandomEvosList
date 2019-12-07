libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.7.0"

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

Compile / unmanagedSources += baseDirectory.value / "../shared/src/main/scala/DexNo.scala"
