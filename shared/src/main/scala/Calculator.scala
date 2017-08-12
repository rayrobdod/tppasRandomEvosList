package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.{Seq, Map}
import scala.collection.mutable.{Buffer => MSeq}
import com.rayrobdod.possibleEvolutions.DexNo.mapCanBuildFrom

/**
 * 
 * 
 * @constructor
 * @param allPokemon A list of every Pokemon
 * @param evolutions A list of evolutions that have occurred in a TPP randomized game
 */
final class Calculator(val allPokemon:Iterable[Pokemon], val evolutions:Map[DexNo, Map[String, Map[EvosGame.Value, DexNo]]]) {
	
	def getPokemon(id:DexNo):Pokemon = allPokemon.find{_.dexNo == id}.get
	val allDexNos:Iterable[DexNo] = allPokemon.map{_.dexNo}
	
	/** A list of prevolutions that have occurred in a TPP randomized game */
	val prevos:Map[DexNo, Seq[(EvosGame.Value, DexNo)]] = {
		val retval = allDexNos.map{dexNo => ((dexNo, MSeq.empty[(EvosGame.Value, DexNo)]))}.toMap
		
		for (
			(prevoDexno, dexnodata) <- this.evolutions;
			(method, evos) <- dexnodata;
			(game, evoDexno) <- evos
		) {
			retval(evoDexno) += ((game, prevoDexno))
		}
		
		retval.mapValues{_.to[Seq]}.map{x => x}
	}
	
	/** A list of each Pokemon's evolutions in a vanila game */
	val naturalEvos:Map[DexNo, Map[String, DexNo]] = {
		evolutions.mapValues{_.mapValues{
			_.get(EvosGame.Natural).getOrElse(DexNo.missing)
		}.map{x => x}}.map{x => x}
	}
	
	private[this] val possibleEvolutionsData:Map[EvosGame.Value, Map[DexNo, Map[String, Seq[Pokemon]]]] = {
		/** True if either `a` value is equal to either `b` value */
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
					val typsMatch = config.monToMatch match {
						case MonToMatch.Neither => true
						case MonToMatch.BaseForm => {
							typesMatch(checkMon.types._1, checkMon.types._2, candidate.types._1, candidate.types._2)
						}
						case MonToMatch.EvolvedForm => {
							typesMatch(naturalEvoMon.types._1, naturalEvoMon.types._2, candidate.types._1, candidate.types._2)
						}
					}
					val bstMatch = config.bstMatches(
						naturalBst = naturalEvoMon.bst,
						candidateBst = candidate.bst
					)
					val expGroupMatch = !config.expGroupMustMatch || candidate.expGrowth == checkMon.expGrowth
					val candidateIsSelf = candidate == checkMon
					val candidateIsNatural = candidate == naturalEvoMon
					
					typsMatch && bstMatch && expGroupMatch &&
							!candidateIsSelf &&
							candidate.exists &&
							(config.naturalEvoAllowed || !candidateIsNatural)
							
				}.to[Seq]}.map{x => x}))
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
		
		retval.mapValues{_.mapValues{_.to[Seq]}.map{x => x}}.map{x => x}
	}
	/** The possible prevolutions that a randomizer can produce for the given mon */
	def possiblePrevolutions(checkNo:DexNo)(implicit config:EvosGame.Value):Seq[Pokemon] = this.possiblePrevolutionsData(config)(checkNo)
	
	
	private[this] val possibleEvosCountData:Map[EvosGame.Value, Map[DexNo, Int]] = this.possibleEvolutionsData.mapValues{_.mapValues{x => x.values.flatten.toSet.size}.map{x => x}}.map{x => x}
	private[this] val possiblePrevosCountData:Map[EvosGame.Value, Map[DexNo, Int]] = this.possiblePrevolutionsData.mapValues{_.mapValues{x => x.size}.map{x => x}}.map{x => x}
	/** The number of possible evolutions that a randomizer can produce for the given mon*/
	def possibleEvosCount(dexno:DexNo)(implicit config:EvosGame.Value):Int = this.possibleEvosCountData(config)(dexno)
	/** The number of possible prevolutions that a randomizer can produce for the given mon*/
	def possiblePrevosCount(dexno:DexNo)(implicit config:EvosGame.Value):Int = this.possiblePrevosCountData(config)(dexno)
	
	private[this] val abridgedEvosData:Map[EvosGame.Value, Map[DexNo, Seq[DexNo]]] = {
		EvosGame.values.map{game =>
			game -> (for (
				(prevoNo, prevonodata) <- this.evolutions.to[Seq];
				(_, methoddata) <- prevonodata.to[Seq];
				evoNo <- methoddata.get(game).to[Seq]
			) yield {
				(prevoNo, evoNo)
			}).groupBy{_._1}.mapValues{_.map{_._2}}.map{x => x}
		}.toMap
	}
	private[this] val threeEvoChainData:Map[EvosGame.Value, Seq[(DexNo, DexNo, DexNo, DexNo)]] = {
		EvosGame.values.map{game =>
			game -> (for (
				(first, secondSeq) <- this.abridgedEvosData(game).to[Seq];
				second <- secondSeq;
				thirdSeq <- this.abridgedEvosData(game).get(second).to[Seq];
				third <- thirdSeq;
				fourthSeq <- this.abridgedEvosData(game).get(third).to[Seq];
				fourth <- fourthSeq
			) yield ((first, second, third, fourth)))
		}.toMap
	}
	def threeEvoChains(implicit config:EvosGame.Value):Seq[(DexNo, DexNo, DexNo, DexNo)] = this.threeEvoChainData(config).sortBy{_._1}
	
	private[this] val finalEvolutionData:Map[EvosGame.Value, Map[DexNo, Seq[DexNo]]] = {
		def followEvoChain(x:DexNo, game:EvosGame.Value):Seq[DexNo] = {
			if (abridgedEvosData(game) contains x) {
				val nexts = this.abridgedEvosData(game)(x)
				nexts.flatMap{y => if (y == x) {Seq(x)} else {followEvoChain(y, game)}}
			} else {
				Seq(x)
			}
		}
		EvosGame.values.map{game =>
			game -> this.allDexNos
					.map{startNum => startNum -> followEvoChain(startNum, game)}
					.filter{x => Seq(x._1) != x._2}
					.toMap
					.map{x => x}
		}.toMap
	}
	def finalEvolutions(mon:DexNo)(implicit config:EvosGame.Value):Seq[DexNo] = finalEvolutionData(config).get(mon).getOrElse(Seq.empty)
	
	private[this] val firstStageMonsData:Map[EvosGame.Value, Set[DexNo]] = {
		EvosGame.values.map{game =>
			game -> (this.allDexNos.filter{no => this.getPokemon(no).exists(game)}.to[Set] -- abridgedEvosData(game).flatMap{_._2}.to[Set])
		}.toMap
	}
	/** AKA Pokemon that nothing evolves into */
	def firstStageMons(implicit config:EvosGame.Value):Set[DexNo] = firstStageMonsData(config)
	
	private[this] val familiesData:Map[EvosGame.Value, Map[DexNo, Set[DexNo]]] = {
		val retval = EvosGame.values.map{config => ((config, allDexNos.map{dexNo => ((dexNo, MSeq.empty[DexNo]))}.toMap))}.toMap
		
		EvosGame.values.foreach{game =>
			this.allDexNos.filter{no => this.getPokemon(no).exists(game)}.foreach{prevoNum =>
				var midNums:List[DexNo] = prevoNum :: Nil
				var finalNums:List[DexNo] = Nil
				
				while (midNums.nonEmpty) {
					val h :: t = midNums
					midNums = t
					
					if (abridgedEvosData(game) contains h) {
						midNums = abridgedEvosData(game)(h).to[List] ::: midNums
						midNums = midNums.filterNot{_ == h}.filterNot{finalNums contains _}
					} else {
						finalNums = h :: finalNums
					}
				}
				
				finalNums.foreach{evoNum =>
					retval(game)(evoNum) += prevoNum
				}
			}
		}
		
		retval.mapValues{families => families.filter{_._2.nonEmpty}.mapValues{_.to[Set]}.map{x => x}}.toMap
	}
	def families(implicit config:EvosGame.Value):Map[DexNo, Set[DexNo]] = familiesData(config)
}

object Calculator {
	import java.io.File
	import java.nio.file.Files
	import com.opencsv.{CSVReader, CSVWriter}
	import java.nio.charset.StandardCharsets.UTF_8
	
	/** Constructs a Calculator from the csv files contained in the `datadir` */
	def fromFiles(datadir:File):Calculator = {
		val allPokemon:Seq[Pokemon] = AllPokemon.apply
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
			val white2 = readEvoDataFile("white2Evolutions.csv", EvosGame.White2)
			
			val summed:Seq[(DexNo, DexNo, String, EvosGame.Value)] = natural ++ alphaSapphire ++ platinum ++ white2
			
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
			}.toMap.map{x => x}
		}
		
		new Calculator(allPokemon, evolutions)
	}
}
