package com.rayrobdod

package object possibleEvolutions {
}

package possibleEvolutions {
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
		final case class Custom(min:Double, max:Double) extends Value {
			def apply(selfBst:Int, naturalBst:Int, candidateBst:Int):Boolean = {
				(naturalBst * max >= candidateBst) && (candidateBst >= naturalBst * min)
			}
			def description = s"From $min to $max of Natural"
		}
		
	}
	
	/**
	 * Notes the time at which the class was compiled. Used for invalidating a cache by `Compiler`.
	 */
	trait CompiledOnNoted {
		/** Returns the time at which the class was compiled */
		def compiledOn:java.time.Instant
	}
}
