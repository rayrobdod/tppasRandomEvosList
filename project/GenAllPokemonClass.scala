package com.rayrobdod.possibleEvolutions

import sbt._
import sbt.Keys._
import sbt.nio.Keys._
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable.Buffer
import scala.language.implicitConversions

object GenAllPokemonClass extends AutoPlugin {
	object autoImport {
		val genAllPokemon = taskKey[Seq[File]]("Build an object containing the known pokemon data")
	}
	import autoImport.genAllPokemon

	override lazy val projectSettings = Seq(
		Compile / genAllPokemon / fileInputs += Glob((Compile / baseDirectory).value.getParentFile / "src" / "main" / "pokemon", "*.tsv"),
		Compile / genAllPokemon / target := (sourceManaged in Compile).value / "AllPokemon.scala",
		Compile / genAllPokemon := {
			val inputs = (Compile / genAllPokemon).inputFiles
			val outFile = (Compile / genAllPokemon / target).value
			val inLines = inputs.flatMap({input => sbt.io.IO.readLines(input.toFile, UTF_8)})
			val outLines = inLines.map({line =>
				val parts = line.split("\t")
				val no = {
					val FORMAT = """(\d+)([A-Z]*)""".r
					parts(0) match {
						case FORMAT(a, "") => s"DexNo.national($a)"
						case FORMAT(a, "A") => s"DexNo.alola($a)"
						case FORMAT("744", "DUSK") => s"DexNo.duskRockruff"
						case _ => s"""DexNo.valueOf("${parts(0)}")"""
					}
				}
				val name = "\"" + parts(1) + "\""

				s"\t\tbuilder += new Pokemon($no, $name, ${parts.drop(2).mkString(", ")})"
			})

			val prefix = """|package com.rayrobdod.possibleEvolutions
				|
				|import scala.collection.immutable.Seq
				|import com.rayrobdod.possibleEvolutions.ExperienceGrowth._
				|import com.rayrobdod.possibleEvolutions.ElementalType._
				|import com.rayrobdod.possibleEvolutions.LegendaryStatus.{Normal => NotLegendary, _}
				|
				|/**
				| * Contains a list of every known Pokémon species.
				| */
				|object AllPokemon {
				|	/** Returns the Pokémon from said list with the specified DexNo */
				|	def get(x:DexNo):Option[Pokemon] = this.apply.find(_.dexNo == x)
				|
				|	val apply:Seq[Pokemon] = {
				|		val builder = Seq.newBuilder[Pokemon]
				|
				|""".stripMargin
			val suffix = """|
				|
				|		builder.result
				|	}
				|}
				|""".stripMargin

			sbt.IO.createDirectory(outFile.getParentFile)
			sbt.IO.write(outFile, prefix ++ outLines.mkString("\r\n") ++ suffix, UTF_8, false)
			Seq(outFile)
		},
		Compile / sourceGenerators += (Compile / genAllPokemon).taskValue
	)
}
