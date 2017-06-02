package com.rayrobdod.possibleEvolutions

import sbt._
import java.io.File
import java.nio.file.Files
import scala.collection.immutable.{Seq, Map}
import scala.collection.mutable.{Buffer => MSeq}
import com.opencsv.{CSVReader, CSVWriter}
import java.nio.charset.StandardCharsets.UTF_8

final class ListOfPokemon(datadir:File) {
	
	val rawdata:Seq[Pokemon] = {
		def readListOfPokemon():Seq[Pokemon] = {
			val inFile = new File(datadir, "listOfPokemon.csv")
			val inReader = new CSVReader(Files.newBufferedReader(inFile.toPath, UTF_8))
			val inData = inReader.readAll.toArray.to[Seq].map{_ match {
				case Array(dexNo:String, name:String, bst1:String, bst2:String, bst6:String, bst7:String, type1:String, type2:String, rpType1:String, rpType2:String) => {
					Pokemon(dexNo.toInt, name, type1, type2, rpType1, rpType2, bst1.toInt, bst2.toInt, bst6.toInt, bst7.toInt)
				}
			}}
			inReader.close()
			inData
		}
		
		readListOfPokemon()
	}
	
	val evolutions:Seq[Map[String, Map[EvosGame.Value, Int]]] = {
		def readEvoDataFile(fileName:String, game:EvosGame.Value):Seq[(Int, Int, String, EvosGame.Value)] = {
			val f = new File(datadir, fileName)
			val r = new CSVReader(Files.newBufferedReader(f.toPath, UTF_8))
			val data = r.readAll.toArray.to[Seq].map{_ match {
				case Array(inNo:String, inName:String, outNo:String, outName:String, method:String) => {
					((inNo.toInt, outNo.toInt, method, game))
				}
			}}
			r.close()
			data
		}
		val natural = readEvoDataFile("naturalEvolutions.csv", EvosGame.Natural)
		val alphaSapphire = readEvoDataFile("alphaSapphireEvolutions.csv", EvosGame.AlphaSapphire)
		val platinum = readEvoDataFile("platinumEvolutions.csv", EvosGame.Platinum)
		
		val summed:Seq[(Int, Int, String, EvosGame.Value)] = natural ++ alphaSapphire ++ platinum
		
		rawdata.map{mon:Pokemon =>
			val methods = natural.filter{_._1 == mon.dexNo}.map{_._3}
			val evos:Map[String, Map[EvosGame.Value, Int]] = methods.map{method:String =>
				((method,
					summed.filter{x => x._1 == mon.dexNo && x._3 == method}
							.map{x => ((x._4, x._2))}
							.toMap
				))
			}.toMap
			evos
		}
	}
	
	val naturalEvos:Seq[Map[String, Int]] = {
		evolutions.map{_.mapValues{x =>
			x.find{_._1 == EvosGame.Natural}.map{_._2}.getOrElse(0)
		}}
	}
	
	val possibleEvolutions:Seq[Map[String, Seq[Pokemon]]] = {
		def typesMatch(a1:String, a2:String, b1:String, b2:String) = {
			a1 == b1 || a1 == b2 || a2 == b1 || a2 == b2
		}
		
		rawdata.map{checkMon =>
			val naturalEvoNos:Map[String, Int] = naturalEvos(checkMon.dexNo)
			val naturalEvoMons:Map[String, Pokemon] = naturalEvoNos.mapValues(rawdata)
			
			// https://github.com/kwsch/pk3DS/blob/master/pk3DS/Subforms/Evolution.cs#L202
			naturalEvoMons.mapValues{naturalEvoMon => rawdata.filter{candidate =>
				(candidate.gen6bst * 6 / 5 > naturalEvoMon.gen6bst) && (naturalEvoMon.gen6bst > candidate.gen6bst * 5 / 6) &&
						typesMatch(naturalEvoMon.type1, naturalEvoMon.type2, candidate.type1, candidate.type2)
			}}
		}
	}
	
	val possiblePrevolutions:Seq[Seq[Pokemon]] = {
		val retval = Seq.fill(rawdata.size){MSeq.empty[Pokemon]}
		
		this.possibleEvolutions.zipWithIndex.foreach({(evos:Map[String, Seq[Pokemon]], prevoDexno:Int) =>
			evos.values.flatten.foreach{evo:Pokemon =>
				retval(evo.dexNo) += this.rawdata(prevoDexno) 
			}
		}.tupled)
		
		retval.map{_.to[Seq]}
	}
	val possibleEvosCount:Seq[Int] = this.possibleEvolutions.map{x => x.values.flatten.toSet.size}
	val possiblePrevosCount:Seq[Int] = this.possiblePrevolutions.map{_.size}
	
}
