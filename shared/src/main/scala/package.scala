package com.rayrobdod

package object possibleEvolutions {
	type Run = Runs.Value
}

package possibleEvolutions {
	object Runs {
		/**
		 * @constructor
		 * @param id the sorting order of the run
		 * @param name the name of the run, suitable to be part of a url
		 * @param shortName the name of the run, suitable for a table header
		 */
		final class Value private[Runs] (val id:Int, val name:String, val shortName:String, val displayName:String) {
			override def toString:String = s"Runs.${name}"
		}
		object Value {
			implicit val RunIdOrdering:scala.math.Ordering[Runs.Value] = Ordering[Int].on(_.id)
		}

		val Natural:Value = new Value(0, "natural", "nat", "Natural")
		val FireRed:Value = new Value(1, "fire-red", "fr", "Fire Red")
		val HeartGold:Value = new Value(2, "heart-gold", "hg", "Heart Gold")
		val AlphaSapphire:Value = new Value(3, "alpha-sapphire", "αS", "Alpha Sapphire")
		val Platinum:Value = new Value(4, "platinum", "rP", "Platinum")
		val White2:Value = new Value(5, "white2", "w2", "White 2")
		val Randy:Value = new Value(6, "randy", "rY", "Y")
		val Colosseum:Value = new Value(7, "colosseum", "co", "Colosseum")
		val UltraMoon:Value = new Value(8, "ultra-moon", "um", "Ultra Moon")
		val ChattyCrystal:Value = new Value(9, "chatty-crystal", "cc", "Chatty Crystal")
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
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean
			/** A textual description of the bstMatches method's function */
			def description:String
		}
		object `Any` extends Value {
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean = true
			def description = "Any"
		}
		object Pk3ds extends Value {
			// https://github.com/kwsch/pk3DS/blob/f0d69b517b8c86ea7a05a9af00bfa6d117de1661/pk3DS/Subforms/Evolution.cs#L198
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean = {
				(candidateBst * 6 / 5 > naturalBst) && (naturalBst > candidateBst * 5 / 6)
			}
			def description = "From ×5/6 (0.833) to ×6/5 (1.20) of Natural"
		}
		object Pk3ds_2 extends Value {
			// https://github.com/kwsch/pk3DS/blob/da0f1f6eca0c76eeb11a1ade8d35b9dc87389179/pk3DS.Core/Randomizers/SpeciesRandomizer.cs#L119
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean = {
				!((naturalBst * 10 / 11 > candidateBst) || (candidateBst > naturalBst * 11 / 10))
			}
			def description = "From ×10/11 (0.909) to ×11/10 (1.10) of Natural"
		}
		object UniversalRandomizer extends Value {
			// https://github.com/Dabomstew/universal-pokemon-randomizer/blob/49e1d38991ee5339400abfc482e08d4cdfc3aacd/src/com/dabomstew/pkrandom/romhandlers/AbstractRomHandler.java#L3011
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean = {
				(naturalBst * 11 / 10 >= candidateBst) && (candidateBst >= naturalBst * 9 / 10)
			}
			def description = "From 90% to 110% of Natural"
		}
		object GoDTool extends Value {
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean = {
				selfBst <= candidateBst
			}
			def description = "Greater Than or Equal To Current"
		}
		object ChattyCrystal extends Value {
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean = {
				(naturalBst + 50 >= candidateBst) && (candidateBst >= naturalBst - 50)
			}
			def description = "Within 50 points of Natural"
		}
		final case class Custom(min:Double, max:Double) extends Value {
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean = {
				(naturalBst * max >= candidateBst) && (candidateBst >= naturalBst * min)
			}
			def description = s"From $min to $max of Natural"
		}
		
	}

	/** A 'mon's experience growth category */
	object ExperienceGrowth extends Enumeration {
		protected class Val(name:String) extends super.Val(name) {
			override def toString = name
		}

		val Fast = new Val("Fast")
		val MedFast = new Val("Medium Fast")
		val SlightlyFast = new Val("Slightly Fast") // fused crystal
		val SlightlySlow = new Val("Slightly Slow") // fused crystal
		val MedSlow = new Val("Medium Slow")
		val Slow = new Val("Slow")
		val Fluctuating = new Val("Fluctuating")
		val Erratic = new Val("Erratic")
	}

	/** A 'mon's elemental type */
	object ElementalType extends Enumeration {
		protected class Val(name:String) extends super.Val(name) {
			override def toString = name
		}

		 // `???` is the value that needs a custom tostring; without it the tostring is
		 // the java version of the file name - `$qmark$qmark$qmark`
		val ??? = new Val("???")
		val Bug = new Val("Bug")
		val Dark = new Val("Dark")
		val Dragon = new Val("Dragon")
		val Electric = new Val("Electric")
		val Fairy = new Val("Fairy")
		val Fighting = new Val("Fighting")
		val Fire = new Val("Fire")
		val Flying = new Val("Flying")
		val Ghost = new Val("Ghost")
		val Grass = new Val("Grass")
		val Ground = new Val("Ground")
		val Ice = new Val("Ice")
		val Normal = new Val("Normal")
		val Poison = new Val("Poison")
		val Psychic = new Val("Psychic")
		val Rock = new Val("Rock")
		val Steel = new Val("Steel")
		val Water = new Val("Water")
	}

	object LegendaryStatus extends Enumeration {
		val Normal, Legendary, Mythical, Shedinja = Value
	}
}
