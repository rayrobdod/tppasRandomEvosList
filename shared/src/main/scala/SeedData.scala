package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.{Seq, Map}
import scala.collection.mutable.{Buffer => MSeq}
import com.rayrobdod.possibleEvolutions.DexNo.mapCanBuildFrom

/**
 * Data about a particular game's true evolutions
 */
abstract class SeedData {
	def game:EvosGame.Value
	def evolutions:Map[DexNo, Map[String, DexNo]]
	
	
	
	private[this] implicit def config = game
	private[this] val extantDexNos = config.knownDexnos
	
	private[this] val abridgedEvosData:Map[DexNo, Seq[DexNo]] = {
		(for (
			(prevoNo, prevonodata) <- this.evolutions.to[Seq];
			(method, evoNo) <- prevonodata.to[Seq]
		) yield {
			(prevoNo, evoNo)
		}).groupBy{_._1}.mapValues{_.map{_._2}}.map{x => x}
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
		def followEvoChain(x:DexNo, game:EvosGame.Value):Seq[DexNo] = {
			if (abridgedEvosData contains x) {
				val nexts = this.abridgedEvosData(x)
				nexts.flatMap{y => if (y == x) {Seq(x)} else {followEvoChain(y, game)}}
			} else {
				Seq(x)
			}
		}
		
		extantDexNos
				.map{startNum => startNum -> followEvoChain(startNum, game)}
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
	final val multiplePrevos:Set[DexNo] = prevos.filter{_._2.size >= 2}.map{_._1}.to[Set]
}
