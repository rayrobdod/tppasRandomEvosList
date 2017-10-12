package com.rayrobdod

import scala.collection.immutable.Seq

package object possibleEvolutions {
	
	val htmlDoctype = scalatags.Text.RawFrag("<!DOCTYPE html>\n")
	
}

package possibleEvolutions {
	
	object EvosGame {
		sealed trait Value {
			def id:Int
			/** This game's name */
			def name:String
			/** An abbreviated version of this game's name */
			def shortName:String
			/** Which Pokemon must the candidate match to be an acceptable candidate */
			def monToMatch:MonToMatch.Value
			/** True if the candidate must have the same EXP group as the prevo to be an acceptable candidate */
			def expGroupMustMatch:Boolean
			/** True if the candidate's BST is close enough to the natural evolution's BST to be an acceptable candidate */
			def bstMatches(naturalBst:Int, candidateBst:Int):Boolean
			/** True if the natural evolution is allowed to be an acceptable candidate */
			def naturalEvoAllowed:Boolean
			
			/** A textual description of the bstMatches method's function */
			def bstMatchString:String
			
			/** True if the game has logs to display and generate data about */
			def seedData:Option[SeedData]
			
			override def toString = this.name
		}
		
		object Natural extends Value {
			override def id:Int = 0
			override def name:String = "natural"
			override def shortName:String = "nat"
			
			override def seedData:Option[SeedData] = Option(evolutionData.Natural)
			
			// Prediction pages aren't built for natural evolutions, so the values used here don't matter
			override def monToMatch:MonToMatch.Value = MonToMatch.BaseForm
			override def expGroupMustMatch:Boolean = true
			override def bstMatches(naturalBst:Int, candidateBst:Int):Boolean = true
			override def bstMatchString:String = "Any"
			override def naturalEvoAllowed:Boolean = true
		}
		
		object AlphaSapphire extends Value {
			override def id:Int = 1
			override def name:String = "alpha-sapphire"
			override def shortName:String = "αS"
			override def monToMatch:MonToMatch.Value = MonToMatch.EvolvedForm
			override def expGroupMustMatch:Boolean = false
			override def seedData:Option[SeedData] = Option(evolutionData.AlphaSapphire)
			// https://github.com/kwsch/pk3DS/blob/f0d69b517b8c86ea7a05a9af00bfa6d117de1661/pk3DS/Subforms/Evolution.cs#L198
			override def bstMatches(naturalBst:Int, candidateBst:Int):Boolean = (candidateBst * 6 / 5 > naturalBst) && (naturalBst > candidateBst * 5 / 6)
			override def bstMatchString:String = "From ×5/6 to ×6/5"
			override def naturalEvoAllowed:Boolean = false
		}
		
		object Platinum extends Value {
			override def id:Int = 2
			override def name:String = "platinum"
			override def shortName:String = "rP"
			override def monToMatch:MonToMatch.Value = MonToMatch.BaseForm
			override def expGroupMustMatch:Boolean = true
			override def seedData:Option[SeedData] = Option(evolutionData.Platinum)
			// https://github.com/Dabomstew/universal-pokemon-randomizer/blob/49e1d38991ee5339400abfc482e08d4cdfc3aacd/src/com/dabomstew/pkrandom/romhandlers/AbstractRomHandler.java#L3011
			override def bstMatches(naturalBst:Int, candidateBst:Int):Boolean = (naturalBst * 11 / 10 >= candidateBst) && (candidateBst >= naturalBst * 9 / 10)
			override def bstMatchString:String = "From 90% to 110%"
			override def naturalEvoAllowed:Boolean = true
		}
		
		object White2 extends Value {
			override def id:Int = 3
			override def name:String = "white2"
			override def shortName:String = "w2"
			override def monToMatch:MonToMatch.Value = MonToMatch.Neither
			override def expGroupMustMatch:Boolean = true
			override def seedData:Option[SeedData] = Option(evolutionData.White2)
			// https://github.com/Dabomstew/universal-pokemon-randomizer/blob/49e1d38991ee5339400abfc482e08d4cdfc3aacd/src/com/dabomstew/pkrandom/romhandlers/AbstractRomHandler.java#L3011
			override def bstMatches(naturalBst:Int, candidateBst:Int):Boolean = (naturalBst * 11 / 10 >= candidateBst) && (candidateBst >= naturalBst * 9 / 10)
			override def bstMatchString:String = "From 90% to 110%"
			override def naturalEvoAllowed:Boolean = false
		}
		
		def values:Seq[Value] = Seq(Natural, AlphaSapphire, Platinum, White2)
	}
	
	object MonToMatch extends Enumeration {
		val BaseForm = Value
		val EvolvedForm = Value
		val Neither = Value
	}
	
}
