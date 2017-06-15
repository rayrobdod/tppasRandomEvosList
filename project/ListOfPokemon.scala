package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.{Seq, Map}
import scala.collection.mutable.{Buffer => MSeq}

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
		evolutions.mapValues{_.mapValues{
			_.get(EvosGame.Natural).getOrElse(DexNo.missing)
		}}
	}
	
	private[this] val possibleEvolutionsData:Map[EvosGame.Value, Map[DexNo, Map[String, Seq[Pokemon]]]] = {
		def typesMatch(a1:String, a2:String, b1:String, b2:String) = {
			a1 == b1 || a1 == b2 || a2 == b1 || a2 == b2
		}
		
		EvosGame.values.map{config =>
			((config, allDexNos.map{checkNo =>
				implicit val config2 = config
				val naturalEvoNos:Map[String, DexNo] = naturalEvos(checkNo)
				val naturalEvoMons:Map[String, Pokemon] = naturalEvoNos.mapValues(this.getPokemon _).map{x => x}
				val checkMon = this.getPokemon(checkNo)
				
				((checkNo, naturalEvoMons.mapValues{naturalEvoMon => allPokemon.filter{candidate =>
					// https://github.com/kwsch/pk3DS/blob/master/pk3DS/Subforms/Evolution.cs#L202
					val typsMatch = config.monToMatch match {
						case MonToMatch.Neither => true
						case MonToMatch.BaseForm => {
							typesMatch(checkMon.types._1, checkMon.types._2, candidate.types._1, candidate.types._2)
						}
						case MonToMatch.EvolvedForm => {
							typesMatch(naturalEvoMon.types._1, naturalEvoMon.types._2, candidate.types._1, candidate.types._2)
						}
					}
					val bstMatch = {
						val candidateBst = candidate.bst
						val selfBst = naturalEvoMon.bst
						(candidateBst * 6 / 5 > selfBst) && (selfBst > candidateBst * 5 / 6)
					}
					val expGroupMatch = !config.expGroupMustMatch || candidate.expGrowth == checkMon.expGrowth
					typsMatch && bstMatch && expGroupMatch
				}}.map{x => x}))
				// `map{x => x}` because `mapValues` creates a view, which is particularly suboptimal with a parameter function this complicated
			}.toMap))
		}.toMap
	}
	/** The possible evolutions that a randomizer can produce for the given mon */
	def possibleEvolutions(checkNo:DexNo)(implicit config:EvosGame.Value):Map[String, Seq[Pokemon]] = this.possibleEvolutionsData(config)(checkNo)
	
	private[this] val possiblePrevolutionsData:Map[EvosGame.Value, Map[DexNo, Seq[Pokemon]]] = {
		val retval = EvosGame.values.map{config => ((config, allDexNos.map{dexNo => ((dexNo, MSeq.empty[Pokemon]))}.toMap))}.toMap
		
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
	def possiblePrevolutions(checkNo:DexNo)(implicit config:EvosGame.Value):Seq[Pokemon] = this.possiblePrevolutionsData(config)(checkNo)
	
	
	private[this] val possibleEvosCountData:Map[EvosGame.Value, Map[DexNo, Int]] = this.possibleEvolutionsData.mapValues{_.mapValues{x => x.values.flatten.toSet.size}}
	private[this] val possiblePrevosCountData:Map[EvosGame.Value, Map[DexNo, Int]] = this.possiblePrevolutionsData.mapValues{_.mapValues{x => x.size}}
	/** The number of possible evolutions that a randomizer can produce for the given mon*/
	def possibleEvosCount(dexno:DexNo)(implicit config:EvosGame.Value):Int = this.possibleEvosCountData(config)(dexno)
	/** The number of possible prevolutions that a randomizer can produce for the given mon*/
	def possiblePrevosCount(dexno:DexNo)(implicit config:EvosGame.Value):Int = this.possiblePrevosCountData(config)(dexno)
	
}

object ListOfPokemon {
	import java.io.File
	import java.nio.file.Files
	import com.opencsv.{CSVReader, CSVWriter}
	import java.nio.charset.StandardCharsets.UTF_8
	
	/** Constructs a ListOfPokemon from the csv files contained in the `datadir` */
	def fromFiles(datadir:File):ListOfPokemon = {
		val allPokemon:Seq[Pokemon] = {
			import scala.collection.JavaConversions.iterableAsScalaIterable
			def readListOfPokemon():Seq[Pokemon] = {
				val inFile = new File(datadir, "listOfPokemon.csv")
				val inReader = new CSVReader(Files.newBufferedReader(inFile.toPath, UTF_8))
				val inData = inReader.readAll.to[Seq].map{_ match {
					case Array(dexNo:String, name:String, bst1:String, bst2:String, bst6:String, bst7:String, type1:String, type2:String, rpType1:String, rpType2:String, expGrowth:String) => {
						// 18 types; 6 exp groups; without interning they'd be represented by 802*5=4010 strings, rathter than 24. Still, it's not like ~100kB really matters, and there's no real speed improvement from interning
						new Pokemon(DexNo(dexNo.toInt), name, type1.intern, type2.intern, rpType1.intern, rpType2.intern, bst1.toInt, bst2.toInt, bst6.toInt, bst7.toInt, expGrowth.intern)
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
