package com.rayrobdod.possibleEvolutions

import sbt._
import Keys._
import java.nio.file.{Path, Files}
import java.nio.charset.StandardCharsets.UTF_8
import com.typesafe.sbt.web.Import.WebKeys.webTarget
import com.typesafe.sbt.web.Import.Assets

object MyBuild {
	
	val monData = TaskKey[ListOfPokemon]("monData")
	val perMonPages = TaskKey[Seq[File]]("perMonPages")
	val perGamePages = TaskKey[Seq[File]]("perGamePages")
	val indexPage = TaskKey[Seq[File]]("indexPage")
	val configuration = SettingKey[Configuration.Value]("evoConfiguration")
	
	val mySettings = Seq(
		configuration := Configuration.Gen5,
		monData := new ListOfPokemon((sourceDirectory in perMonPages in Assets).value),
		
		target in perMonPages in Assets := (resourceManaged in Assets).value,
		sourceDirectory in perMonPages in Assets := (baseDirectory).value / "src" / "data",
		perMonPages in Assets := {
			val tarDir = (target in perMonPages in Assets).value
			val allMons = monData.value
			allMons.allDexNos.map{x =>
				(streams in perMonPages in Assets).value.log.info(x.toString)
				val outFile = (tarDir / (x.toString + ".html")).toPath
				val output = PageTemplates.perMonPage(x, allMons)(configuration.value).toString
				val output2 = java.util.Collections.singleton(output)
				Files.createDirectories(outFile.getParent)
				Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
				outFile.toFile
			}
		},
		resourceGenerators in Assets += (perMonPages in Assets).taskValue,
		perGamePages in Assets := {
			val tarDir = (target in perMonPages in Assets).value
			val allMons = monData.value
			EvosGame.values.to[Seq].map{x =>
				(streams in perMonPages in Assets).value.log.info(x.toString)
				val outFile = (tarDir / (x.toString + ".html")).toPath
				val output = PageTemplates.perGamePage(x, allMons).toString
				val output2 = java.util.Collections.singleton(output)
				Files.createDirectories(outFile.getParent)
				Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
				outFile.toFile
			}
		},
		resourceGenerators in Assets += (perGamePages in Assets).taskValue,
		indexPage in Assets := {
			val tarDir = (target in perMonPages in Assets).value
			val baseDir = (baseDirectory).value
			val allMons = monData.value
			val outFile = (tarDir / ("index.html")).toPath
			val output = PageTemplates.index(allMons, readPrologue(baseDir / "README.md"))(configuration.value).toString
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		},
		resourceGenerators in Assets += (indexPage in Assets).taskValue
	)
}
