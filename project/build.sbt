libraryDependencies += "com.opencsv" % "opencsv" % "3.4"
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.5"

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
