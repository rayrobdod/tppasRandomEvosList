package com.rayrobdod.possibleEvolutions

/**
 * Information about a particular species of Pokémon
 */
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
		DexNo.missing < this.dexNo && this.dexNo <= config.maxKnownDexno 
	}
	
	// Create the tuples only once, reducing GC churn
	private[this] val rpTypes = ((rpType1, rpType2))
	private[this] val natTypes = ((naturalType1, naturalType2))
	private[this] val noFairyTypes = {
		val _1 = (if (naturalType1 == "Fairy") {"Normal"} else {naturalType1})
		val _2 = (if (naturalType2 == "Fairy") {_1} else {naturalType2})
		((_1, _2))
	}
	
	def types(implicit config:EvosGame.Value):(String, String) = config.typeType match {
		case MonTypeType.RandPlat => rpTypes
		case MonTypeType.Natural => natTypes
		case MonTypeType.NoFairy => noFairyTypes
	}
	
	def bst(implicit config:EvosGame.Value):Int = config.bstType match {
		case MonBstType.Gen1 => gen1bst
		case MonBstType.Gen2 => gen2bst
		case MonBstType.Gen6 => gen6bst
		case MonBstType.Gen7 => gen7bst
	}
}

object Pokemon {
	implicit val dexNoOrdering = Ordering.by{x:Pokemon => x.dexNo}
}
