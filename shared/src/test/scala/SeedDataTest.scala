package com.rayrobdod.possibleEvolutions

import org.scalatest.FunSpec

class SeedDataTest extends FunSpec {
	
	describe("SeedData") {
		describe("abridgedEvosData") {
			it ("works") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(1) -> Map("a" -> DexNo.national(2)),
						DexNo.national(2) -> Map("a" -> DexNo.national(3)),
						DexNo.national(3) -> Map("a" -> DexNo.national(4)),
					)
				}
				assertResult(Seq( (DexNo.national(1), DexNo.national(2), DexNo.national(3), DexNo.national(4)) )){dut.threeEvoChains}
			}
		}
		describe("finalEvolutions") {
			it ("single") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(1) -> Map("a" -> DexNo.national(2))
					)
				}
				assertResult(Seq(DexNo.national(2))){dut.finalEvolutions(DexNo.national(1))}
			}
			it ("double") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(1) -> Map("a" -> DexNo.national(2)),
						DexNo.national(2) -> Map("a" -> DexNo.national(3))
					)
				}
				assertResult(Seq(DexNo.national(3))){dut.finalEvolutions(DexNo.national(1))}
				assertResult(Seq(DexNo.national(3))){dut.finalEvolutions(DexNo.national(2))}
			}
			it ("split evo") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(1) -> Map("a" -> DexNo.national(2), "b" -> DexNo.national(3))
					)
				}
				assertResult(Seq(DexNo.national(2), DexNo.national(3))){dut.finalEvolutions(DexNo.national(1))}
			}
		}
		describe("firstStageMons") {
			it ("when no evos, then all are first stage") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map.empty
				}
				assertResult(dut.game.knownDexnos.to[Set]){dut.firstStageMons}
			}
			it ("asdf") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(100) -> Map("a" -> DexNo.national(200))
					)
				}
				assertResult(false)(dut.firstStageMons contains DexNo.national(200))
			}
		}
		describe("families") {
			it ("works") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(1) -> Map("a" -> DexNo.national(2)),
						DexNo.national(3) -> Map("a" -> DexNo.national(4)),
						DexNo.national(2) -> Map("a" -> DexNo.national(5)),
						DexNo.national(4) -> Map("a" -> DexNo.national(5))
					)
				}
				assertResult(Set(
					DexNo.national(1), DexNo.national(2),
					DexNo.national(3), DexNo.national(4),
					DexNo.national(5)
				)){dut.families(DexNo.national(5))}
				assertResult(false){dut.families contains DexNo.national(2)}
			}
		}
		describe("prevos") {
			it ("works") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(1) -> Map("a" -> DexNo.national(2)),
						DexNo.national(3) -> Map("a" -> DexNo.national(4)),
						DexNo.national(2) -> Map("a" -> DexNo.national(5)),
						DexNo.national(4) -> Map("a" -> DexNo.national(5))
					)
				}
				assertResult(Map(
					DexNo.national(5) -> Set(DexNo.national(2), DexNo.national(4)),
					DexNo.national(4) -> Set(DexNo.national(3)),
					DexNo.national(2) -> Set(DexNo.national(1))
				)){dut.prevos}
			}
		}
		describe("multiplePrevos") {
			it ("single") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(1) -> Map("a" -> DexNo.national(2))
					)
				}
				assertResult(Set.empty){dut.multiplePrevos}
			}
			it ("pair") {
				object dut extends SeedData {
					def game = EvosGame.Natural
					def evolutions = Map(
						DexNo.national(1) -> Map("a" -> DexNo.national(3)),
						DexNo.national(2) -> Map("a" -> DexNo.national(3))
					)
				}
				assertResult(Set(DexNo.national(3))){dut.multiplePrevos}
			}
		}
	}
}
