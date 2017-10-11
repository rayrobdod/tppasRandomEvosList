package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.Map

/**
 * Data about a particular game's true evolutions
 */
abstract class EvolutionData {
	val game:EvosGame.Value
	val apply:Map[DexNo, Map[String, DexNo]]
	
}
