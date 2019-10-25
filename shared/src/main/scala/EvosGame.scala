package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.Seq

/**
 * Describes the randomizer settings for a particular game.
 */
object EvosGame {
	sealed trait Value {
		/** Internal id for e.g. sorting. Also the order in which said games were played. */
		def id:Int
		/** This game's name */
		def name:String
		/** An abbreviated version of this game's name */
		def shortName:String


		/** The dexnos of extant pokemon */
		def knownDexnos:Seq[DexNo]
		/** the set of BST values used by Pokemon */
		def bstType:MonBstType.Value
		/** The set of types used by Pokemon */
		def typeType:MonTypeType.Value


		/** Which Pokemon must the candidate match to be an acceptable candidate */
		def monToMatch:MonTypeToMatch.Value
		/** True if the candidate must have the same EXP group as the prevo to be an acceptable candidate */
		def expGroupMustMatch:Boolean
		/** True if the natural evolution is allowed to be an acceptable candidate */
		def naturalEvoAllowed:Boolean
		/** True if  */
		def remainingStageMatch:Boolean
		/** True if evolution into a legendary is allowed */
		def legendaryAllowed:Boolean
		/**
		 * A function that compares a natural and candidate Pokemon to determine whether
		 * the candidate is an acceptable evolution candidate
		 */
		def bstMatchFunction:BstMatchFunction.Value

		/** True if the game has logs to display and generate data about */
		def seedData:Option[SeedData]

		override def toString = this.name
	}

	object Natural extends Value {
		override def id:Int = 0
		override def name:String = "natural"
		override def shortName:String = "nat"

		override def seedData:Option[SeedData] = Option(evolutionData.Natural)
		override def knownDexnos:Seq[DexNo] = AllPokemon.apply.map{_.dexNo}

		// Prediction pages aren't built for natural evolutions, so the values used here don't matter
		override def monToMatch:MonTypeToMatch.Value = MonTypeToMatch.BaseForm
		override def expGroupMustMatch:Boolean = true
		override def remainingStageMatch:Boolean = true
		override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.Any
		override def naturalEvoAllowed:Boolean = true
		override def legendaryAllowed:Boolean = true
		override def bstType:MonBstType.Value = MonBstType.Gen7
		override def typeType:MonTypeType.Value = MonTypeType.Natural
	}

	object AlphaSapphire extends Value {
		override def id:Int = 1
		override def name:String = "alpha-sapphire"
		override def shortName:String = "αS"
		override def monToMatch:MonTypeToMatch.Value = MonTypeToMatch.EvolvedForm
		override def expGroupMustMatch:Boolean = false
		override def remainingStageMatch:Boolean = false
		override def seedData:Option[SeedData] = Option(evolutionData.AlphaSapphire)
		override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.Pk3ds
		override def naturalEvoAllowed:Boolean = false
		override def legendaryAllowed:Boolean = true
		override def knownDexnos:Seq[DexNo] = DexNo.NationalDexNoRange(1, 721)
		override def bstType:MonBstType.Value = MonBstType.Gen6
		override def typeType:MonTypeType.Value = MonTypeType.Natural
	}

	object Platinum extends Value {
		override def id:Int = 2
		override def name:String = "platinum"
		override def shortName:String = "rP"
		override def monToMatch:MonTypeToMatch.Value = MonTypeToMatch.BaseForm
		override def expGroupMustMatch:Boolean = true
		override def remainingStageMatch:Boolean = false
		override def seedData:Option[SeedData] = Option(evolutionData.Platinum)
		override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.UniversalRandomizer
		override def naturalEvoAllowed:Boolean = true
		override def legendaryAllowed:Boolean = true
		override def knownDexnos:Seq[DexNo] = DexNo.NationalDexNoRange(1, 493)
		override def bstType:MonBstType.Value = MonBstType.Gen2
		override def typeType:MonTypeType.Value = MonTypeType.RandPlat
	}

	object White2 extends Value {
		override def id:Int = 3
		override def name:String = "white2"
		override def shortName:String = "w2"
		override def monToMatch:MonTypeToMatch.Value = MonTypeToMatch.Neither
		override def expGroupMustMatch:Boolean = true
		override def remainingStageMatch:Boolean = false
		override def seedData:Option[SeedData] = Option(evolutionData.White2)
		override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.UniversalRandomizer
		override def naturalEvoAllowed:Boolean = false
		override def legendaryAllowed:Boolean = true
		override def knownDexnos:Seq[DexNo] = DexNo.NationalDexNoRange(1, 649)
		override def bstType:MonBstType.Value = MonBstType.Gen2
		override def typeType:MonTypeType.Value = MonTypeType.NoFairy
	}

	object Randy extends Value {
		override def id:Int = 4
		override def name:String = "randy"
		override def shortName:String = "rY"
		override def monToMatch:MonTypeToMatch.Value = MonTypeToMatch.Neither
		override def expGroupMustMatch:Boolean = false
		override def remainingStageMatch:Boolean = false
		override def seedData:Option[SeedData] = Option(evolutionData.Randy)
		override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.Pk3ds_2
		override def naturalEvoAllowed:Boolean = true
		override def legendaryAllowed:Boolean = false
		override def knownDexnos:Seq[DexNo] = DexNo.NationalDexNoRange(1, 721)
		override def bstType:MonBstType.Value = MonBstType.Gen6
		override def typeType:MonTypeType.Value = MonTypeType.Natural
	}

	object Colosseum extends Value {
		override def id:Int = 5
		override def name:String = "colosseum"
		override def shortName:String = "co"
		override def monToMatch:MonTypeToMatch.Value = MonTypeToMatch.EvolvedForm
		override def expGroupMustMatch:Boolean = false
		override def remainingStageMatch:Boolean = true
		override def seedData:Option[SeedData] = None //Option(evolutionData.Colosseum)
		override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.GoDTool
		override def naturalEvoAllowed:Boolean = false
		override def legendaryAllowed:Boolean = true
		override def knownDexnos:Seq[DexNo] = DexNo.NationalDexNoRange(1, 386)
		override def bstType:MonBstType.Value = MonBstType.Gen2
		override def typeType:MonTypeType.Value = MonTypeType.NoFairy
	}

	object UltraMoon extends Value {
		override def id:Int = 6
		override def name:String = "ultra-moon"
		override def shortName:String = "um"
		override def monToMatch:MonTypeToMatch.Value = MonTypeToMatch.Neither
		override def expGroupMustMatch:Boolean = false
		override def remainingStageMatch:Boolean = true
		override def seedData:Option[SeedData] = None //Option(evolutionData.UltraMoon)
		override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.Pk3ds_2
		override def naturalEvoAllowed:Boolean = true
		override def legendaryAllowed:Boolean = false
		override def knownDexnos:Seq[DexNo] = DexNo.NationalDexNoRange(1, 807) ++ DexNo.alolanDexNos :+ DexNo.duskRockruff
		override def bstType:MonBstType.Value = MonBstType.Gen7
		override def typeType:MonTypeType.Value = MonTypeType.Natural
	}

	final case class Custom(
		val knownDexnos:Seq[DexNo],
		val bstType:MonBstType.Value,
		val typeType:MonTypeType.Value,
		val monToMatch:MonTypeToMatch.Value,
		val bstMatchFunction:BstMatchFunction.Value,
		val expGroupMustMatch:Boolean,
		val naturalEvoAllowed:Boolean,
		val remainingStageMatch:Boolean,
		val legendaryAllowed:Boolean,
	) extends Value {
		override def id:Int = -1
		override def name:String = "custom"
		override def shortName:String = "cm"
		override def seedData:Option[SeedData] = None
	}
}
