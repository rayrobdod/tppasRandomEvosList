package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.{Seq, Map, SortedMap}
import scala.collection.mutable.{Buffer => MSeq}

/**
 * Data about a particular game's true evolutions
 */
abstract class SeedData {
	def evolutions:Map[DexNo, Map[String, DexNo]]
	def extantDexNos:Seq[DexNo]
	
	/** The evolution data, but ignoring the method used to evolve */
	private[this] val abridgedEvosData:Map[DexNo, Seq[DexNo]] = {
		this.evolutions.mapValues{_.values.to[Seq]}.map{x => x}
	}
	
	private[this] val threeEvoChainData:Seq[(DexNo, DexNo, DexNo, DexNo)] = {
		(for (
			(first, secondSeq) <- this.abridgedEvosData.to[Seq];
			second <- secondSeq;
			thirdSeq <- this.abridgedEvosData.get(second).to[Seq];
			third <- thirdSeq;
			fourthSeq <- this.abridgedEvosData.get(third).to[Seq];
			fourth <- fourthSeq
		) yield ((first, second, third, fourth)))
	}
	final def threeEvoChains:Seq[(DexNo, DexNo, DexNo, DexNo)] = this.threeEvoChainData.sortBy{_._1}
	
	private[this] val finalEvolutionData:Map[DexNo, Seq[DexNo]] = {
		def followEvoChain(x:DexNo):Seq[DexNo] = {
			if (abridgedEvosData contains x) {
				val nexts = this.abridgedEvosData(x)
				nexts.flatMap{y => if (y == x) {Seq(x)} else {followEvoChain(y)}}
			} else {
				Seq(x)
			}
		}
		
		extantDexNos
				.map{startNum => startNum -> followEvoChain(startNum)}
				.filter{x => Seq(x._1) != x._2}
				.toMap
				.map{x => x}
	}
	final def finalEvolutions(mon:DexNo):Seq[DexNo] = finalEvolutionData.get(mon).getOrElse(Seq.empty)
	
	private[this] val firstStageMonsData:Set[DexNo] = {
		extantDexNos.to[Set] -- abridgedEvosData.flatMap{_._2}.to[Set]
	}
	/** AKA Pokemon that nothing evolves into */
	final def firstStageMons:Set[DexNo] = firstStageMonsData
	
	private[this] val familiesData:Map[DexNo, Set[DexNo]] = {
		val retval = extantDexNos.map{dexNo => ((dexNo, MSeq.empty[DexNo]))}.toMap
		
		extantDexNos.foreach{prevoNum =>
			var midNums:List[DexNo] = prevoNum :: Nil
			var finalNums:List[DexNo] = Nil
			
			while (midNums.nonEmpty) {
				val h :: t = midNums
				midNums = t
				
				if (abridgedEvosData contains h) {
					midNums = abridgedEvosData(h).to[List] ::: midNums
					midNums = midNums.filterNot{_ == h}.filterNot{finalNums contains _}
				} else {
					finalNums = h :: finalNums
				}
			}
			
			finalNums.foreach{evoNum =>
				retval(evoNum) += prevoNum
			}
		}
		
		retval.filter{_._2.nonEmpty}.mapValues{_.to[Set]}.map{x => x}
	}
	/** Key: the evolution; Value: the recursive prevolutions */
	final def families:Map[DexNo, Set[DexNo]] = familiesData
	
	private[this] val prevosData:Map[DexNo, Set[DexNo]] = {
		val retval = extantDexNos.map{dexNo => ((dexNo, MSeq.empty[DexNo]))}.toMap
		
		for (
			(prevo, evos) <- abridgedEvosData.to[Seq];
			evo <- evos.to[Seq]
		) {
			retval(evo) += prevo
		}
		
		retval.filter{_._2.nonEmpty}.mapValues{_.to[Set]}.map{x => x}
	}
	/** Key: the evolution; Value: the immediate prevolutions */
	final def prevos:Map[DexNo, Set[DexNo]] = this.prevosData
	
	/** Pokemon with multiple prevos */
	final val multiplePrevos:Set[DexNo] = prevos.filter{_._2.size >= 2}.keySet
}

object SeedData {
	val runToValue:SortedMap[Run, SeedData] = SortedMap(
		Runs.Natural -> evolutionData.Natural,
		Runs.AlphaSapphire -> evolutionData.AlphaSapphire,
		Runs.Platinum -> evolutionData.Platinum,
		Runs.White2 -> evolutionData.White2,
		Runs.Randy -> evolutionData.Randy,
	)
}
