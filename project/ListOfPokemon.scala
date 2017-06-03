package com.rayrobdod.possibleEvolutions

import sbt._
import java.io.File
import java.nio.file.Files
import scala.collection.immutable.{Seq, Map}
import scala.collection.mutable.{Buffer => MSeq}
import com.opencsv.{CSVReader, CSVWriter}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * @constructor
 * @param allPokemon A list of every Pokemon
 * @param evolutions A list of evolutions that have occured in a TPP randomized game
 */
final class ListOfPokemon(val allPokemon:Seq[Pokemon], val evolutions:Map[DexNo, Map[String, Map[EvosGame.Value, DexNo]]]) {
	
	def getPokemon(id:DexNo):Pokemon = allPokemon.find{_.dexNo == id}.get
	val allDexNos:Seq[DexNo] = allPokemon.map{_.dexNo}
	
	/** A list of prevolutions that have occured in a TPP randomized game */
	val prevos:Map[DexNo, Seq[(EvosGame.Value, DexNo)]] = {
		val retval = allDexNos.map{dexNo => ((dexNo, MSeq.empty[(EvosGame.Value, DexNo)]))}.toMap
		
		for (
			(prevoDexno, dexnodata) <- this.evolutions;
			(method, evos) <- dexnodata;
			(game, evoDexno) <- evos
		) {
			retval(evoDexno) += ((game, prevoDexno))
		}
		
		retval.mapValues{_.to[Seq]}
	}
	
	/** A list of each Pokemon's evolutions in a vanila game */
	val naturalEvos:Map[DexNo, Map[String, DexNo]] = {
		evolutions.mapValues{_.mapValues{x =>
			x.find{_._1 == EvosGame.Natural}.map{_._2}.getOrElse(DexNo.missing)
		}}
	}
	
	private[this] val possibleEvolutionsData:Map[Configuration.Value, Map[DexNo, Map[String, Seq[Pokemon]]]] = {
		def typesMatch(a1:String, a2:String, b1:String, b2:String) = {
			a1 == b1 || a1 == b2 || a2 == b1 || a2 == b2
		}
		
		Configuration.values.map{config =>
			((config, allDexNos.map{checkNo =>
				implicit val config2 = config
				val naturalEvoNos:Map[String, DexNo] = naturalEvos(checkNo)
				val naturalEvoMons:Map[String, Pokemon] = naturalEvoNos.mapValues(this.getPokemon _)
				val checkMon = this.getPokemon(checkNo)
				
				// https://github.com/kwsch/pk3DS/blob/master/pk3DS/Subforms/Evolution.cs#L202
				((checkNo, naturalEvoMons.mapValues{naturalEvoMon => allPokemon.filter{candidate =>
					val typsMatch = {
						val candidateType = candidate.types
						val selfType = config.monToMatch match {
							case MonToMatch.BaseForm => checkMon.types
							case MonToMatch.EvolvedForm => naturalEvoMon.types 
						}
						typesMatch(selfType._1, selfType._2, candidateType._1, candidateType._2)
					}
					val bstMatch = {
						val candidateBst = candidate.bst
						val selfBst = naturalEvoMon.bst
						(candidateBst * 6 / 5 > selfBst) && (selfBst > candidateBst * 5 / 6)
					}
					typsMatch && bstMatch
				}}))
			}.toMap))
		}.toMap
	}
	/** The possible evolutions that a randomizer can produce for the given mon */
	def possibleEvolutions(checkNo:DexNo)(implicit config:Configuration.Value):Map[String, Seq[Pokemon]] = this.possibleEvolutionsData(config)(checkNo)
	
	private[this] val possiblePrevolutionsData:Map[Configuration.Value, Map[DexNo, Seq[Pokemon]]] = {
		val retval = Configuration.values.map{config => ((config, allDexNos.map{dexNo => ((dexNo, MSeq.empty[Pokemon]))}.toMap))}.toMap
		
		for (
			(config, configdata) <- this.possibleEvolutionsData;
			(prevoDexno, dexnodata) <- configdata;
			(method, evos) <- dexnodata;
			evo <- evos
		) {
			retval(config)(evo.dexNo) += this.getPokemon(prevoDexno)
		}
		
		retval.mapValues{_.mapValues{_.to[Seq]}}
	}
	/** The possible prevolutions that a randomizer can produce for the given mon */
	def possiblePrevolutions(checkNo:DexNo)(implicit config:Configuration.Value):Seq[Pokemon] = this.possiblePrevolutionsData(config)(checkNo)
	
	
	private[this] val possibleEvosCountData:Map[Configuration.Value, Map[DexNo, Int]] = this.possibleEvolutionsData.mapValues{_.mapValues{x => x.values.flatten.toSet.size}}
	private[this] val possiblePrevosCountData:Map[Configuration.Value, Map[DexNo, Int]] = this.possiblePrevolutionsData.mapValues{_.mapValues{x => x.size}}
	/** The number of possible evolutions that a randomizer can produce for the given mon*/
	def possibleEvosCount(dexno:DexNo)(implicit config:Configuration.Value):Int = this.possibleEvosCountData(config)(dexno)
	/** The number of possible prevolutions that a randomizer can produce for the given mon*/
	def possiblePrevosCount(dexno:DexNo)(implicit config:Configuration.Value):Int = this.possiblePrevosCountData(config)(dexno)
	
}

object ListOfPokemon {
	
	/** Constructs a ListOfPokemon from the csv files contained in the `datadir` */
	def fromFiles(datadir:File):ListOfPokemon = {
		val allPokemon:Seq[Pokemon] = {
			def readListOfPokemon():Seq[Pokemon] = {
				val inFile = new File(datadir, "listOfPokemon.csv")
				val inReader = new CSVReader(Files.newBufferedReader(inFile.toPath, UTF_8))
				val inData = inReader.readAll.toArray.to[Seq].map{_ match {
					case Array(dexNo:String, name:String, bst1:String, bst2:String, bst6:String, bst7:String, type1:String, type2:String, rpType1:String, rpType2:String) => {
						new Pokemon(DexNo(dexNo.toInt), name, type1, type2, rpType1, rpType2, bst1.toInt, bst2.toInt, bst6.toInt, bst7.toInt)
					}
				}}
				inReader.close()
				inData
			}
			
			readListOfPokemon()
		}
		val allDexNos:Seq[DexNo] = allPokemon.map{_.dexNo}
		
		val evolutions:Map[DexNo, Map[String, Map[EvosGame.Value, DexNo]]] = {
			def readEvoDataFile(fileName:String, game:EvosGame.Value):Seq[(DexNo, DexNo, String, EvosGame.Value)] = {
				val f = new File(datadir, fileName)
				val r = new CSVReader(Files.newBufferedReader(f.toPath, UTF_8))
				val data = r.readAll.toArray.to[Seq].map{_ match {
					case Array(inNo:String, inName:String, outNo:String, outName:String, method:String) => {
						((DexNo(inNo.toInt), DexNo(outNo.toInt), method, game))
					}
				}}
				r.close()
				data
			}
			val natural = readEvoDataFile("naturalEvolutions.csv", EvosGame.Natural)
			val alphaSapphire = readEvoDataFile("alphaSapphireEvolutions.csv", EvosGame.AlphaSapphire)
			val platinum = readEvoDataFile("platinumEvolutions.csv", EvosGame.Platinum)
			
			val summed:Seq[(DexNo, DexNo, String, EvosGame.Value)] = natural ++ alphaSapphire ++ platinum
			
			allDexNos.map{dexNo:DexNo =>
				val methods = natural.filter{_._1 == dexNo}.map{_._3}
				val evos:Map[String, Map[EvosGame.Value, DexNo]] = methods.map{method:String =>
					((method,
						summed.filter{x => x._1 == dexNo && x._3 == method}
								.map{x => ((x._4, x._2))}
								.toMap
					))
				}.toMap
				((dexNo, evos))
			}.toMap
		}
		
		new ListOfPokemon(allPokemon, evolutions)
	}
}
