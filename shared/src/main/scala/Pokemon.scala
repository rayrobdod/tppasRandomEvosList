package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.Seq

final class Pokemon(
	  val dexNo:DexNo = DexNo.undef
	, val name:String = ""
	, naturalType1:String = ""
	, naturalType2:String = ""
	, rpType1:String = ""
	, rpType2:String = ""
	, gen1bst:Int = -1
	, gen2bst:Int = -1
	, gen6bst:Int = -1
	, gen7bst:Int = -1
	, val expGrowth:String = ""
) {
	def exists(implicit config:EvosGame.Value):Boolean = { 
		val maxNo = config match {
			case EvosGame.Platinum => 493
			case EvosGame.White2 => 649
			case EvosGame.AlphaSapphire => 721
			case EvosGame.Natural => 802
		}
		DexNo.missing < this.dexNo && this.dexNo <= maxNo 
	}
	
	// Create the tuples only once, reducing GC churn
	private[this] val rpTypes = ((rpType1, rpType2))
	private[this] val natTypes = ((naturalType1, naturalType2))
	private[this] val noFairyTypes = {
		val _1 = (if (naturalType1 == "Fairy") {"Normal"} else {naturalType1})
		val _2 = (if (naturalType2 == "Fairy") {_1} else {naturalType2})
		((_1, _2))
	}
	
	def types(implicit config:EvosGame.Value):(String, String) = config match {
		case EvosGame.Platinum => rpTypes
		case EvosGame.AlphaSapphire | EvosGame.Natural => natTypes
		case EvosGame.White2 => noFairyTypes
	}
	
	def bst(implicit config:EvosGame.Value):Int = config match {
		case EvosGame.Natural => gen7bst
		case EvosGame.Platinum => gen2bst
		case EvosGame.White2 => gen2bst
		case EvosGame.AlphaSapphire => gen6bst
	}
}

object Pokemon {
	implicit val dexNoOrdering = Ordering.by{x:Pokemon => x.dexNo}
}
