package com.rayrobdod.possibleEvolutions

import sbt._
import sbt.Keys._
import sbt.nio.Keys._
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import scala.language.implicitConversions
import scala.collection.mutable.{Buffer, Map => MMap, SortedMap => MSortedMap}

/**
 */
object GenEvolutionsSeedDataClassPlugin extends AutoPlugin {
	def prefix(objectName:String):String = s"""|
		|package com.rayrobdod.possibleEvolutions
		|package seedData
		|
		|import scala.collection.immutable.{ListMap, Map, Seq, SortedMap}
		|
		|object ${objectName} extends SeedData {
		|	lazy val evolutions:Map[DexNo, Map[String, DexNo]] = {
		|		val builder = SortedMap.newBuilder[DexNo, Map[String, DexNo]]
		|""".stripMargin
	def suffix(extantDexNos:String):String = s"""|
		|		builder.result
		|	}
		|
		|	override def extantDexNos:Seq[DexNo] = ${extantDexNos}
		|}
		|""".stripMargin

	def dexNoUnapplySeq(xs:String):String = {
		xs.split(',').map({x =>
			val NATIONAL_RANGE = """(\d+)\-(\d+)""".r
			val GENX = """(Gen\d)""".r

			x match {
				case "All" => "AllPokemon.apply.map{_.dexNo}"
				case GENX(x) => s"DexNoSets.$x"
			}
		}).reduce{_ + " ++ " + _}
	}

	object autoImport {
		val seedDataToScala = taskKey[Seq[File]]("")
	}
	import autoImport.seedDataToScala
	override lazy val projectSettings = Seq(
		Compile / seedDataToScala / sourceDirectories := Seq.empty,
		Compile / seedDataToScala / sourceDirectories += (Compile / sourceDirectory).value / "seedData",
		Compile / seedDataToScala / sourceDirectories += (Compile / baseDirectory).value / ".." / "src" / "main" / "seedData",
		Compile / seedDataToScala / fileInputs := (Compile / seedDataToScala / sourceDirectories).value.map(x => Glob(x.toPath, "*.evolutions")),
		Compile / seedDataToScala / target := (Compile / crossTarget).value / "seedData",
		Compile / seedDataToScala := {
			val inputs = (Compile / seedDataToScala).inputFiles
			val outFolder = (Compile / seedDataToScala / target).value

			def compile(filename:String, data:java.util.List[String]):String = {
				var currentHeader:String = ""
				var dexNos:String = ""
				val evos:MSortedMap[DexNo, Buffer[(String, DexNo)]] = MSortedMap.empty[DexNo, Buffer[(String, DexNo)]]

				val HEADER = """== (\w+) ==""".r
				val EVO_LINE = """([^\t]+)\t(\d+[A-Z]*)""".r

				data.stream().forEach(_ match {
					case "" =>
						{} // ignore blank lines
					case HEADER(x) =>
						currentHeader = x
					case x if currentHeader == "DexNos" =>
						dexNos = dexNoUnapplySeq(x)
					case EVO_LINE(method, result) => {
						val from = DexNo.valueOf(currentHeader)
						val to = DexNo.valueOf(result)
						evos.getOrElseUpdate(from, Buffer.empty) += ((method, to))
					}
				})

				val evosStr = evos.map({x =>
					val (from, methods) = x
					val methods2 = methods.map({y =>
						val (method, to) = y
						s""" "${method}" -> ${dexNoSource(to)},"""
					})
					val prefix = s"builder += ((${dexNoSource(from)}, ListMap(\n\t\t\t"
					val suffix = "\n\t\t\t)))"
					methods2.mkString(prefix, "\n\t\t\t", suffix)
				}).mkString("\n\t\t", "\n\t\t", "\n")

				prefix(filename) + evosStr + suffix(dexNos)
			}

			val result = inputs.map({input =>
				val inFileName = input.getFileName.toString
				val className = {
					val mainPart = inFileName.split("\\.").apply(0)
					mainPart.updated(0, mainPart(0).toUpper)
				}
				val inLines = java.nio.file.Files.readAllLines(input, UTF_8)

				((outFolder / (className + ".scala"), compile(className, inLines)))
			})
			result.foreach({x => sbt.io.IO.write(x._1, x._2, UTF_8)})
			result.map(_._1)
		},
		Compile / managedSourceDirectories += (Compile / seedDataToScala / target).value,
		Compile / sourceGenerators += (Compile / seedDataToScala).taskValue,
	)
}
