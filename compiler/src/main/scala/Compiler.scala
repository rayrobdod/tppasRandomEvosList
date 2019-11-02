package com.rayrobdod.possibleEvolutions

import java.io.File
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.Seq

object Compiler {
	private final implicit class FileWithSlash(f:File) {
		def /(x:String) = new File(f, x)
	}
	private[this] val gamesToMakePagesAbout = Seq(
		Runs.AlphaSapphire,
		Runs.Platinum,
		Runs.White2,
		Runs.Randy,
		Runs.Colosseum,
		Runs.UltraMoon,
	)
	private[this] val predictors = gamesToMakePagesAbout.map{game => ((game, new Predictor(RandomizerSettings.runToValue(game))))}
	private[this] val bracketsWithLineBreak = ">" + System.lineSeparator + "<"
	
	final case class Context(
		generatePerMonPages:java.lang.Boolean,
		targetDirectory:File
	)
	
	final case class Result(
		files:Array[File]
	)
	
	
	private[this] def writeToFile(outRelPath:String, contents:() => String)(ctx:Context):File = {
		val outFile = (ctx.targetDirectory / outRelPath).toPath
		val outData = java.util.Collections.singleton(contents.apply.replace("><", bracketsWithLineBreak))
		Files.createDirectories(outFile.getParent)
		Files.write(outFile, outData, UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
		outFile.toFile
	}
	
	def apply(ctx:Context):Result = {
		val perMonPages:Seq[File] = {
			for (
				(game, predictions) <- predictors if ctx.generatePerMonPages;
				mon <- predictions.extantPokemon
			) yield {
				val x = mon.dexNo
				writeToFile(s"${game.name}/${x}.html", () => PageTemplatesText.perMonPage(x, predictions, game).render)(ctx)
			}
		}
		
		val perGamePages:Seq[File] = {
			predictors.map{case (game, predictor) =>
				writeToFile(s"${game.name}/index.html", () => PageTemplatesText.perGamePage(predictor, game).render)(ctx)
			}
		}
		
		val otherPages:Seq[File] = Seq(
			writeToFile("index.html", () => PageTemplatesText.index(IndexPrologue.apply, gamesToMakePagesAbout.map{_.name}).render)(ctx),
			writeToFile("shared/index.html", () => PageTemplatesText.sharedPage().render)(ctx),
			writeToFile("shared/133.html", () => PageTemplatesText.sharedEeveePage(SeedData.runToValue.keys.to[Seq]).render)(ctx),
			writeToFile("shared/fossil.html", () => PageTemplatesText.sharedFossilPage().render)(ctx),
			writeToFile("theoretical/index.html", () => PageTemplatesText.theoreticalFormPage.render)(ctx),
			writeToFile("theoretical/results.html", () => PageTemplatesText.theoreticalPage.render)(ctx),
		)
		
		Result(
			files = Seq(otherPages, perGamePages, perMonPages).flatten.toArray
		)
	}
}
