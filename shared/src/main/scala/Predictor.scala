package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.{Seq, Map, Iterable}
import scala.collection.mutable.{Buffer => MSeq}

/**
 * For a given game's settings, list the likely post-randomization evolutions
 */
final class Predictor(game:EvosGame.Value) {
	private[this] implicit val config:EvosGame.Value = game
	
	private[this] val knownDexNos = game.knownDexnos
	val extantPokemon:Iterable[Pokemon] = knownDexNos.map{num => AllPokemon.get(num).get}
	def getPokemon(id:DexNo):Pokemon = extantPokemon.find{_.dexNo == id}.get
	
	private[this] val naturalEvos = evolutionData.Natural.evolutions
			.filter{case (k,_) => knownDexNos.contains(k)}
			.map{case (k,v) => ((k, v.filter{case (_, v2) => knownDexNos.contains(v2)}))}
	
	private[this] val possibleEvolutionsData:Map[DexNo, Map[String, Seq[Pokemon]]] = {
		/** True if either `a` value is equal to either `b` value */
		def typesMatch(a1:String, a2:String, b1:String, b2:String) = {
			a1 == b1 || a1 == b2 || a2 == b1 || a2 == b2
		}
		
		knownDexNos.map{checkNo =>
			val naturalEvoNos:Map[String, DexNo] = naturalEvos.get(checkNo).getOrElse(Map.empty)
			val naturalEvoMons:Map[String, Pokemon] = naturalEvoNos.mapValues(this.getPokemon _).map{x => x}
			val checkMon = this.getPokemon(checkNo)
			
			((checkNo, naturalEvoMons.mapValues{naturalEvoMon => extantPokemon.filter{candidate =>
				val typsMatch = config.monToMatch match {
					case MonTypeToMatch.Neither => true
					case MonTypeToMatch.BaseForm => {
						typesMatch(checkMon.types._1, checkMon.types._2, candidate.types._1, candidate.types._2)
					}
					case MonTypeToMatch.EvolvedForm => {
						typesMatch(naturalEvoMon.types._1, naturalEvoMon.types._2, candidate.types._1, candidate.types._2)
					}
				}
				val bstMatch = config.bstMatchFunction(
					naturalBst = naturalEvoMon.bst,
					candidateBst = candidate.bst
				)
				val expGroupMatch = !config.expGroupMustMatch || candidate.expGrowth == checkMon.expGrowth
				val candidateIsSelf = candidate == checkMon
				val candidateIsNatural = candidate == naturalEvoMon
				
				typsMatch && bstMatch && expGroupMatch &&
						!candidateIsSelf &&
						(config.naturalEvoAllowed || !candidateIsNatural)
						
			}.to[Seq]}.map{x => x}))
			// `map{x => x}` because `mapValues` creates a view, which is particularly suboptimal with a parameter function this complicated
		}.toMap
	}
	/** The possible evolutions that a randomizer can produce for the given mon */
	def possibleEvolutions(checkNo:DexNo):Map[String, Seq[Pokemon]] = this.possibleEvolutionsData(checkNo)
	
	private[this] val possiblePrevolutionsData:Map[DexNo, Seq[Pokemon]] = {
		val retval = knownDexNos.map{dexNo => ((dexNo, MSeq.empty[Pokemon]))}.toMap
		
		for (
			(prevoDexno, dexnodata) <- this.possibleEvolutionsData;
			(method, evos) <- dexnodata;
			evo <- evos
		) {
			retval(evo.dexNo) += this.getPokemon(prevoDexno)
		}
		
		retval.mapValues{_.to[Seq]}.map{x => x}
	}
	/** The possible prevolutions that a randomizer can produce for the given mon */
	def possiblePrevolutions(checkNo:DexNo):Seq[Pokemon] = this.possiblePrevolutionsData(checkNo)
	
	
	private[this] val possibleEvosCountData:Map[DexNo, Int] = this.possibleEvolutionsData.mapValues{x => x.values.flatten.toSet.size}.map{x => x}
	private[this] val possiblePrevosCountData:Map[DexNo, Int] = this.possiblePrevolutionsData.mapValues{x => x.size}.map{x => x}
	/** The number of possible evolutions that a randomizer can produce for the given mon*/
	def possibleEvosCount(dexno:DexNo):Int = this.possibleEvosCountData(dexno)
	/** The number of possible prevolutions that a randomizer can produce for the given mon*/
	def possiblePrevosCount(dexno:DexNo):Int = this.possiblePrevosCountData(dexno)
}
