package com.rayrobdod.possibleEvolutions

import java.io.File
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.Seq

object Compiler {
	private final implicit class FileWithSlash(f:File) {
		def /(x:String) = new File(f, x)
	}
	private[this] val gamesToMakePagesAbout = Seq(EvosGame.AlphaSapphire, EvosGame.Platinum, EvosGame.White2)
	private[this] val seedDatas = evolutionData.Natural +: gamesToMakePagesAbout.flatMap{_.seedData}
	private[this] val predictors = gamesToMakePagesAbout.map{game => ((game, new Predictor(game)))}
	
	final case class Context(
		generatePerMonPages:java.lang.Boolean,
		targetDirectory:File
	)
	
	final case class Result(
		files:Array[File]
	)
	
	
	def apply(ctx:Context):Result = {
		val perMonPages:Seq[File] = {
			if (ctx.generatePerMonPages) {
				predictors.flatMap{case (game, predictions) =>
					predictions.extantPokemon.map{_.dexNo}.map{x =>
						System.out.println(s"${game.name}/${x}")
						val outFile = (ctx.targetDirectory / game.name / s"${x}.html").toPath
						val output = PageTemplatesText.perMonPage(x, predictions, game, seedDatas).render
						val output2 = java.util.Collections.singleton(output)
						Files.createDirectories(outFile.getParent)
						Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
						outFile.toFile
					}
				}
			} else {
				Seq.empty
			}
		}
		
		val perGamePages:Seq[File] = {
			predictors.map{case (game, predictor) =>
				val outFile = (ctx.targetDirectory / game.name / "index.html").toPath
				val output = PageTemplatesText.perGamePage(predictor, game).render
				val output2 = java.util.Collections.singleton(output)
				Files.createDirectories(outFile.getParent)
				Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
				outFile.toFile
			}
		}
		
		val sharedPage:Seq[File] = {
			val outFile = (ctx.targetDirectory / "shared" / "index.html").toPath
			val output = PageTemplatesText.sharedPage(seedDatas).render
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		}
		
		val sharedEeveePage:Seq[File] = {
			val outFile = (ctx.targetDirectory / "shared" / "133.html").toPath
			val output = PageTemplatesText.sharedEeveePage(seedDatas).render
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		}
		
		val indexPage:Seq[File] = {
			val prologue = IndexPrologue.apply
			val outFile = (ctx.targetDirectory / "index.html").toPath
			val output = PageTemplatesText.index(prologue, gamesToMakePagesAbout.map{_.name}).render
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		}
		
		val theoreticalPage:Seq[File] = {
			val outFile = (ctx.targetDirectory / "theoretical" / "index.html").toPath
			val output = PageTemplatesText.theoreticalPage.render
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		}
		
		Result(
			files = (indexPage ++: sharedPage ++: theoreticalPage ++: perGamePages ++: perMonPages ++: sharedEeveePage).toArray
		)
	}
}