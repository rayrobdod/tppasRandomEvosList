package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.{Seq, SortedMap}

/**
 * Describes the randomizer settings for a particular game.
 * @constructor
 * @param knownDexnos The range of mon within the randomizer's domain
 * @param bstType The category of mon bst used by the randomizer
 * @param typeType The category mon elemental types used by the randomizer
 *
 * @param typeMatch mon elemental types filter
 * @param expGroupMustMatch True if the candidate must have the same EXP group as the prevo to be an acceptable candidate
 * @param naturalEvoAllowed True if the natural evolution is allowed to be an acceptable candidate
 * @param remainingStageMatch True if the candidate must have one less remaining evolution stage remaining than the base mon to be an acceptable candidate
 * @param legendaryAllowed True if evolution into a legendary is allowed
 * @param bstMatchFunction bst filter
 */
final case class RandomizerSettings(
	val knownDexnos:Seq[DexNo],
	val bstType:MonBstType.Value,
	val typeType:MonTypeType.Value,
	val typeMatch:MonTypeToMatch.Value,
	val expGroupMustMatch:Boolean,
	val naturalEvoAllowed:Boolean,
	val remainingStageMatch:Boolean,
	val legendaryAllowed:Boolean,
	val bstMatchFunction:BstMatchFunction.Value,
)

object RandomizerSettings {
	private[this] val AlphaSapphire = new RandomizerSettings(
		typeMatch = MonTypeToMatch.EvolvedForm,
		expGroupMustMatch = false,
		remainingStageMatch = false,
		bstMatchFunction = BstMatchFunction.Pk3ds,
		naturalEvoAllowed = false,
		legendaryAllowed = true,
		knownDexnos = DexNo.NationalDexNoRange(1, 721),
		bstType = MonBstType.Gen6,
		typeType = MonTypeType.Natural,
	)

	private[this] val Platinum = new RandomizerSettings(
		typeMatch = MonTypeToMatch.BaseForm,
		expGroupMustMatch = true,
		remainingStageMatch = false,
		bstMatchFunction = BstMatchFunction.UniversalRandomizer,
		naturalEvoAllowed = true,
		legendaryAllowed = true,
		knownDexnos = DexNo.NationalDexNoRange(1, 493),
		bstType = MonBstType.Gen2,
		typeType = MonTypeType.RandPlat,
	)

	private[this] val White2 = new RandomizerSettings(
		typeMatch = MonTypeToMatch.Neither,
		expGroupMustMatch = true,
		remainingStageMatch = false,
		bstMatchFunction = BstMatchFunction.UniversalRandomizer,
		naturalEvoAllowed = false,
		legendaryAllowed = true,
		knownDexnos = DexNo.NationalDexNoRange(1, 649),
		bstType = MonBstType.Gen2,
		typeType = MonTypeType.NoFairy,
	)

	private[this] val Randy = new RandomizerSettings(
		typeMatch = MonTypeToMatch.Neither,
		expGroupMustMatch = false,
		remainingStageMatch = false,
		bstMatchFunction = BstMatchFunction.Pk3ds_2,
		naturalEvoAllowed = true,
		legendaryAllowed = false,
		knownDexnos = DexNo.NationalDexNoRange(1, 721),
		bstType = MonBstType.Gen6,
		typeType = MonTypeType.Natural,
	)

	private[this] val Colosseum = new RandomizerSettings(
		typeMatch = MonTypeToMatch.EvolvedForm,
		expGroupMustMatch = false,
		remainingStageMatch = true,
		bstMatchFunction = BstMatchFunction.GoDTool,
		naturalEvoAllowed = false,
		legendaryAllowed = true,
		knownDexnos = DexNo.NationalDexNoRange(1, 386),
		bstType = MonBstType.Gen2,
		typeType = MonTypeType.NoFairy,
	)

	private[this] val UltraMoon = new RandomizerSettings(
		typeMatch = MonTypeToMatch.Neither,
		expGroupMustMatch = false,
		remainingStageMatch = true,
		bstMatchFunction = BstMatchFunction.Pk3ds_2,
		naturalEvoAllowed = true,
		legendaryAllowed = false,
		knownDexnos = DexNo.NationalDexNoRange(1, 807) ++ DexNo.alolanDexNos :+ DexNo.duskRockruff,
		bstType = MonBstType.Gen7,
		typeType = MonTypeType.Natural,
	)

	val runToValue:SortedMap[Run, RandomizerSettings] = SortedMap(
		Runs.AlphaSapphire -> AlphaSapphire,
		Runs.Platinum -> Platinum,
		Runs.White2 -> White2,
		Runs.Randy -> Randy,
		Runs.Colosseum -> Colosseum,
		Runs.UltraMoon -> UltraMoon,
	)
}
