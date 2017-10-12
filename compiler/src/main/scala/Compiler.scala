package com.rayrobdod.possibleEvolutions

import java.io.File
import java.nio.file.{Path, Files}
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.Seq

object Compiler {
	private final implicit class FileWithSlash(f:File) {
		def /(x:String) = new File(f, x)
	}
	private[this] val gamesToMakePagesAbout = Seq(EvosGame.AlphaSapphire, EvosGame.Platinum, EvosGame.White2)
	
	final case class Context(
		  sourceDirectory:File
		, targetDirectory:File
	)
	
	final case class Result(
		  files:Array[File]
	)
	
	
	def apply(ctx:Context):Result = {
		val calculator = Calculator.fromFiles(ctx.sourceDirectory)
		
		val perMonPages:Seq[File] = {
			gamesToMakePagesAbout.flatMap{game =>
				implicit val config = game
				calculator.allDexNos.filter{dexNo => calculator.getPokemon(dexNo).exists}.map{x =>
					System.out.println(s"${game.name}/${x}")
					val outFile = (ctx.targetDirectory / game.name / s"${x}.html").toPath
					val output = PageTemplates.perMonPage(x, calculator)(config).render
					val output2 = java.util.Collections.singleton(output)
					Files.createDirectories(outFile.getParent)
					Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
					outFile.toFile
				}
			}
		}
		
		val perGamePages:Seq[File] = {
			gamesToMakePagesAbout.map{game =>
				val predictor = new Predictor(game)
				val outFile = (ctx.targetDirectory / game.name / "index.html").toPath
				val output = PageTemplates.perGamePage(predictor, game).render
				val output2 = java.util.Collections.singleton(output)
				Files.createDirectories(outFile.getParent)
				Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
				outFile.toFile
			}
		}
		
		val sharedPage:Seq[File] = {
			val outFile = (ctx.targetDirectory / "shared" / "index.html").toPath
			val output = PageTemplates.sharedPage(calculator).render
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		}
		
		val indexPage:Seq[File] = {
			val prologue = IndexPrologue.apply
			val outFile = (ctx.targetDirectory / "index.html").toPath
			val output = PageTemplates.index(prologue).render
			val output2 = java.util.Collections.singleton(output)
			Files.createDirectories(outFile.getParent)
			Files.write(outFile, output2, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
			Seq(outFile.toFile)
		}
		
		Result(
			files = (indexPage ++: sharedPage ++: perGamePages ++: perMonPages).toArray
		)
	}
}