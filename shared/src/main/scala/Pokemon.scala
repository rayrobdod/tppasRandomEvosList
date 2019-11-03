package com.rayrobdod.possibleEvolutions

/**
 * Information about a particular species of Pokémon
 */
final class Pokemon(
	val dexNo:DexNo,
	val name:String,
	naturalType1:ElementalType.Value,
	naturalType2:ElementalType.Value,
	rpType1:ElementalType.Value,
	rpType2:ElementalType.Value,
	gen1bst:Int,
	gen2bst:Int,
	gen6bst:Int,
	gen7bst:Int,
	val expGrowth:ExperienceGrowth.Value,
	val isLegendary:LegendaryStatus.Value,
) {
	// Create the tuples only once, reducing GC churn
	private[this] val rpTypes = ((rpType1, rpType2))
	private[this] val natTypes = ((naturalType1, naturalType2))
	private[this] val noFairyTypes = {
		val _1 = (if (naturalType1 == ElementalType.Fairy) {ElementalType.Normal} else {naturalType1})
		val _2 = (if (naturalType2 == ElementalType.Fairy) {_1} else {naturalType2})
		((_1, _2))
	}
	
	def types(implicit typeType:MonTypeType.Value):(ElementalType.Value, ElementalType.Value) = typeType match {
		case MonTypeType.RandPlat => rpTypes
		case MonTypeType.Natural => natTypes
		case MonTypeType.NoFairy => noFairyTypes
	}
	
	def bst(implicit bstType:MonBstType.Value):Int = bstType match {
		case MonBstType.Gen1 => gen1bst
		case MonBstType.Gen2 => gen2bst
		case MonBstType.Gen6 => gen6bst
		case MonBstType.Gen7 => gen7bst
	}
}

object Pokemon {
	implicit val dexNoOrdering = Ordering.by{x:Pokemon => x.dexNo}
}
