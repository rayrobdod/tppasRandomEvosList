package com.rayrobdod.possibleEvolutions

import sbt._
import Keys._
import java.nio.file.{Path, Files}
import java.nio.charset.StandardCharsets.UTF_8
import com.typesafe.sbt.web.Import.WebKeys.webTarget
import com.typesafe.sbt.web.Import.Assets

object MyBuild {
	
	private[this] val gameToMakePagesAbout = Seq(EvosGame.AlphaSapphire, EvosGame.Platinum, EvosGame.White2)
	
	val monData = TaskKey[ListOfPokemon]("monData")
	val perMonPages = TaskKey[Seq[File]]("perMonPages")
	val perGamePages = TaskKey[Seq[File]]("perGamePages")
	val sharedPage = TaskKey[Seq[File]]("sharedPage")
	val indexPage = TaskKey[Seq[File]]("indexPage")
	
	val mySettings = Seq(
		monData := ListOfPokemon.fromFiles((sourceDirectory in perMonPages in Assets).value),
		(resourceDirectories in Assets) += (resourceManaged in Assets).value,
		
		target in perMonPages in Assets := (resourceManaged in Assets).value,
		sourceDirectory in perMonPages in Assets := (baseDirectory).value / "src" / "main" / "data",
		perMonPages in Assets := {
			val tarDir = (target in perMonPages in Assets).value
			val allMons = monData.value
			gameToMakePagesAbout.flatMap{game =>
				implicit val config = game
				allMons.allDexNos.filter{dexNo => allMons.getPokemon(dexNo).exists}.map{x =>
					(streams in perMonPages in Assets).value.log.info(s"${game}/${x}")
					val outFile = (tarDir / game.toString / s"${x}.html").toPath
					val output = PageTemplates.perMonPage(x, allMons)(config).toString
					val output2 = java.util.Collections.singleton(output)
					Files.createDirectories(outFile.getParent)
					Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
					outFile.toFile
				}
			}
		},
		resourceGenerators in Assets += (perMonPages in Assets).taskValue,
		perGamePages in Assets := {
			val tarDir = (target in perMonPages in Assets).value
			val allMons = monData.value
			gameToMakePagesAbout.map{x =>
				val outFile = (tarDir / x.toString / "index.html").toPath
				val output = PageTemplates.perGamePage(x, allMons).toString
				val output2 = java.util.Collections.singleton(output)
				Files.createDirectories(outFile.getParent)
				Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
				outFile.toFile
			}
		},
		resourceGenerators in Assets += (perGamePages in Assets).taskValue,
		sharedPage in Assets := {
			val tarDir = (target in perMonPages in Assets).value
			val baseDir = (baseDirectory).value
			val allMons = monData.value
			val outFile = (tarDir / "shared" / ("index.html")).toPath
			val output = PageTemplates.sharedPage(allMons).toString
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		},
		resourceGenerators in Assets += (sharedPage in Assets).taskValue,
		indexPage in Assets := {
			val tarDir = (target in perMonPages in Assets).value
			val baseDir = (baseDirectory).value
			val allMons = monData.value
			val outFile = (tarDir / ("index.html")).toPath
			val output = PageTemplates.index(readPrologue(baseDir / "README.md")).toString
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		},
		resourceGenerators in Assets += (indexPage in Assets).taskValue
	)
}
