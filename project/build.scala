package com.rayrobdod.possibleEvolutions

import sbt._
import Keys._
import java.nio.file.{Path, Files}
import java.nio.charset.StandardCharsets.UTF_8
import com.typesafe.sbt.web.Import.WebKeys.webTarget
import com.typesafe.sbt.web.Import.Assets

object MyBuild extends Build {
	
	val perMonPages = TaskKey[Seq[File]]("perMonPages")
	val indexPage = TaskKey[Seq[File]]("indexPage")
	
	val mySettings = Seq(
		target in perMonPages in Assets := (resourceManaged in Assets).value,
		sourceDirectory in perMonPages in Assets := (baseDirectory).value / "src" / "data",
		perMonPages in Assets := {
			val tarDir = (target in perMonPages in Assets).value
			val allMons = getAllPokemon((sourceDirectory in perMonPages in Assets).value)
			allMons.map{_.dexNo}.map{x =>
				PageTemplates.evosCacheReadDir = (sourceDirectory in perMonPages in Assets).value
				(streams in perMonPages in Assets).value.log.info(x.toString)
				val outFile = (tarDir / (x.toString + ".html")).toPath
				val output = PageTemplates.perMonPage(x, allMons).toString
				val output2 = java.util.Collections.singleton(output)
				Files.createDirectories(outFile.getParent)
				Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
				outFile.toFile
			}
		},
		resourceGenerators in Assets <+= perMonPages in Assets,
		indexPage in Assets := {
			PageTemplates.evosCacheReadDir = (sourceDirectory in perMonPages in Assets).value
			val tarDir = (target in perMonPages in Assets).value
			val baseDir = (baseDirectory).value
			val allMons = getAllPokemon((sourceDirectory in perMonPages in Assets).value).tail
			val outFile = (tarDir / ("index.html")).toPath
			val output = PageTemplates.index(allMons, readPrologue(baseDir / "README.md")).toString
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		},
		resourceGenerators in Assets <+= indexPage in Assets
	)
	
	lazy val root = Project(
			id = "randEvos",
			base = file("."),
			settings = {
				mySettings ++
				Nil
			}
	).enablePlugins(com.typesafe.sbt.web.SbtWeb)
}