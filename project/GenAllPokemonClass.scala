package com.rayrobdod.possibleEvolutions

import sbt._
import sbt.Keys._
import sbt.nio.Keys._
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable.Buffer
import scala.language.implicitConversions

object dexNoSource {
	private[this] val DUSKROCKRUFF = DexNo.duskRockruff
	private[this] val RARESINISTEA = DexNo.antiqueSinistea
	private[this] val WATERURSHIFU = DexNo.waterUrshifu
	def apply(x:DexNo) = x match {
		case DUSKROCKRUFF => "DexNo.duskRockruff"
		case RARESINISTEA => "DexNo.antiqueSinistea"
		case WATERURSHIFU => "DexNo.waterUrshifu"
		case DexNo.national(y) => s"DexNo.national(${y})"
		case DexNo.alola(y) => s"DexNo.alola(${y})"
		case DexNo.galar(y) => s"DexNo.galar(${y})"
		case DexNo.fused(y) => s"DexNo.fused(${y})"
		case _ => s"""DexNo.valueOf("${x}")"""
	}
}

object GenAllPokemonClassPlugin extends AutoPlugin {
	private[this] val PARTITION_SIZE = 300

	object autoImport {
		val genAllPokemon = taskKey[Seq[File]]("Build an object containing the known pokemon data")
	}
	import autoImport.genAllPokemon

	override lazy val projectSettings = Seq(
		Compile / genAllPokemon / fileInputs += Glob((Compile / baseDirectory).value.getParentFile / "src" / "main" / "pokemon", "*.tsv"),
		Compile / genAllPokemon / target := (Compile / sourceManaged).value / "AllPokemon.scala",
		Compile / genAllPokemon := {
			val inputs = (Compile / genAllPokemon).inputFiles
			val outFile = (Compile / genAllPokemon / target).value
			val inLines = inputs.flatMap({input => sbt.io.IO.readLines(input.toFile, UTF_8)})
			val outLines = inLines.map({line =>
				val parts = line.split("\t")
				val no = dexNoSource(DexNo.valueOf(parts(0)))
				val name = "\"" + parts(1) + "\""

				s"\t\tbuilder += new Pokemon($no, $name, ${parts.drop(2).mkString(", ")})"
			})

			val prefix = """|package com.rayrobdod.possibleEvolutions
				|
				|import scala.collection.immutable.Seq
				|import scala.collection.mutable.Builder
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
			val apply = (0 until outLines.grouped(PARTITION_SIZE).size).map(idx => s"\t\tthis.apply_impl_$idx(builder)").mkString("\r\n")
			val middle = """|
				|
				|		builder.result
				|	}
				|
				|""".stripMargin
			val impls = outLines.grouped(PARTITION_SIZE).zipWithIndex.map({groupIdx =>
				val (group, idx) = groupIdx
				val prefix = s"\tprivate[this] def apply_impl_$idx(builder:Builder[Pokemon, Seq[Pokemon]]) = {\r\n"
				val suffix = "\r\n\t}\r\n"
				group.mkString(prefix, "\r\n", suffix)
			}).mkString("\r\n")
			val suffix = """|
				|}
				|""".stripMargin

			sbt.IO.createDirectory(outFile.getParentFile)
			sbt.IO.write(outFile, prefix ++ apply ++ middle ++ impls ++ suffix, UTF_8, false)
			Seq(outFile)
		},
		Compile / sourceGenerators += (Compile / genAllPokemon).taskValue
	)
}
