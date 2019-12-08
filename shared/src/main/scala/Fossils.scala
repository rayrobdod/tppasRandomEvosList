package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.SortedMap

object Fossils extends Enumeration {
	val Amber, Helix, Dome, Root, Claw, Skull, Armor,
			Cover, Plume, Jaw, Sail = Value
	val BirdDrake, BirdDino, FishDrake, FishDino = Value

	lazy val seedData:SortedMap[Run, SortedMap[Fossils.Value, DexNo]] = SortedMap(
		Runs.Natural -> SortedMap(
			Fossils.Amber -> DexNo.national(142),
			Fossils.Helix -> DexNo.national(138),
			Fossils.Dome -> DexNo.national(140),
			Fossils.Root -> DexNo.national(345),
			Fossils.Claw -> DexNo.national(347),
			Fossils.Skull -> DexNo.national(408),
			Fossils.Armor -> DexNo.national(410),
			Fossils.Cover -> DexNo.national(564),
			Fossils.Plume -> DexNo.national(566),
			Fossils.Jaw -> DexNo.national(696),
			Fossils.Sail -> DexNo.national(698),
			Fossils.BirdDrake -> DexNo.national(880),
			Fossils.BirdDino -> DexNo.national(881),
			Fossils.FishDrake -> DexNo.national(882),
			Fossils.FishDino -> DexNo.national(883),
		),
		Runs.FireRed -> SortedMap(
			Fossils.Amber -> DexNo.national(6),
			Fossils.Helix -> DexNo.national(211),
			Fossils.Dome -> DexNo.national(168),
		),
		Runs.HeartGold -> SortedMap(
			Fossils.Amber -> DexNo.national(288),
			Fossils.Helix -> DexNo.national(200),
			Fossils.Dome -> DexNo.national(132) /*?*/,
			Fossils.Root -> DexNo.national(399) /*?*/,
			Fossils.Claw -> DexNo.national(466),
			Fossils.Skull -> DexNo.national(2) /*?*/,
			Fossils.Armor -> DexNo.national(180) /*?*/,
		),
		Runs.Platinum -> SortedMap(
			Fossils.Amber -> DexNo.national(207),
			Fossils.Helix -> DexNo.national(17),
			Fossils.Dome -> DexNo.national(431),
			Fossils.Root -> DexNo.national(94),
			Fossils.Claw -> DexNo.national(290),
			Fossils.Skull -> DexNo.national(109),
			Fossils.Armor -> DexNo.national(31),
		),
		Runs.White2 -> SortedMap(
			Fossils.Amber -> DexNo.national(472),
			Fossils.Helix -> DexNo.national(531),
			Fossils.Dome -> DexNo.national(152),
			Fossils.Root -> DexNo.national(399),
			Fossils.Claw -> DexNo.national(570),
			Fossils.Skull -> DexNo.national(432),
			Fossils.Armor -> DexNo.national(256),
			Fossils.Cover -> DexNo.national(195),
			Fossils.Plume -> DexNo.national(163),
		),
	)
}
