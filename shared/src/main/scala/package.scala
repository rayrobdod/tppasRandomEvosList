package com.rayrobdod

import scala.collection.immutable.Seq

package object possibleEvolutions {
}

package possibleEvolutions {
	
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
			override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.Any
			override def naturalEvoAllowed:Boolean = true
			override def bstType:MonBstType.Value = MonBstType.Gen7
			override def typeType:MonTypeType.Value = MonTypeType.Natural
		}
		
		object AlphaSapphire extends Value {
			override def id:Int = 1
			override def name:String = "alpha-sapphire"
			override def shortName:String = "αS"
			override def monToMatch:MonTypeToMatch.Value = MonTypeToMatch.EvolvedForm
			override def expGroupMustMatch:Boolean = false
			override def seedData:Option[SeedData] = Option(evolutionData.AlphaSapphire)
			override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.Pk3ds
			override def naturalEvoAllowed:Boolean = false
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
			override def seedData:Option[SeedData] = Option(evolutionData.Platinum)
			override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.UniversalRandomizer
			override def naturalEvoAllowed:Boolean = true
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
			override def seedData:Option[SeedData] = Option(evolutionData.White2)
			override def bstMatchFunction:BstMatchFunction.Value = BstMatchFunction.UniversalRandomizer
			override def naturalEvoAllowed:Boolean = false
			override def knownDexnos:Seq[DexNo] = DexNo.NationalDexNoRange(1, 649)
			override def bstType:MonBstType.Value = MonBstType.Gen2
			override def typeType:MonTypeType.Value = MonTypeType.NoFairy
		}
		
		final case class Custom(
			  val knownDexnos:Seq[DexNo]
			, val bstType:MonBstType.Value
			, val typeType:MonTypeType.Value
			, val monToMatch:MonTypeToMatch.Value
			, val bstMatchFunction:BstMatchFunction.Value
			, val expGroupMustMatch:Boolean
			, val naturalEvoAllowed:Boolean
		) extends Value {
			override def id:Int = -1
			override def name:String = "custom"
			override def shortName:String = "cm"
			override def seedData:Option[SeedData] = None
		}
	}
	
	/** Which pokemon the randomized evolution's type must match */
	object MonTypeToMatch extends Enumeration {
		val BaseForm = Value
		val EvolvedForm = Value
		val Neither = Value
	}
	
	/** Which type system did the game use */
	object MonTypeType extends Enumeration {
		val Natural = Value
		val NoFairy = Value
		val RandPlat = Value
	}
	
	/** Which bst values did the game use */
	object MonBstType extends Enumeration {
		val Gen1 = Value
		val Gen2 = Value
		val Gen6 = Value
		val Gen7 = Value
	}
	
	object BstMatchFunction {
		sealed trait Value {
			/** True if the candidate's BST is close enough to the natural evolution's BST to be an acceptable candidate */
			def apply(naturalBst:Int, candidateBst:Int):Boolean
			/** A textual description of the bstMatches method's function */
			def description:String
		}
		object `Any` extends Value {
			def apply(naturalBst:Int, candidateBst:Int):Boolean = true
			def description = "Any"
		}
		object Pk3ds extends Value {
			// https://github.com/kwsch/pk3DS/blob/f0d69b517b8c86ea7a05a9af00bfa6d117de1661/pk3DS/Subforms/Evolution.cs#L198
			def apply(naturalBst:Int, candidateBst:Int):Boolean = {
				(candidateBst * 6 / 5 > naturalBst) && (naturalBst > candidateBst * 5 / 6)
			}
			def description = "From ×5/6 to ×6/5"
		}
		object UniversalRandomizer extends Value {
			// https://github.com/Dabomstew/universal-pokemon-randomizer/blob/49e1d38991ee5339400abfc482e08d4cdfc3aacd/src/com/dabomstew/pkrandom/romhandlers/AbstractRomHandler.java#L3011
			def apply(naturalBst:Int, candidateBst:Int):Boolean = {
				(naturalBst * 11 / 10 >= candidateBst) && (candidateBst >= naturalBst * 9 / 10)
			}
			def description = "From 90% to 110%"
		}
		final case class Custom(min:Double, max:Double) extends Value {
			def apply(naturalBst:Int, candidateBst:Int):Boolean = {
				(naturalBst * max >= candidateBst) && (candidateBst >= naturalBst * min)
			}
			def description = s"From $min to $max"
		}
		
	}
	
}
