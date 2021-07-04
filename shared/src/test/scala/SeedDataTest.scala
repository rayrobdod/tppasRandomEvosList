package com.rayrobdod.possibleEvolutions

import org.scalatest.funspec.AnyFunSpec

class SeedDataTest extends AnyFunSpec {
	final class MySeedData(datas:Map[Int, Seq[Int]]) extends SeedData {
		override def extantDexNos = DexNoSets.NationalRange(1, 100)
		override def evolutions = datas.map({x =>
			val (from, tos) = x
			val from2 = DexNo.national(from)
			val tos2 = tos.map({to => s"$to" -> DexNo.national(to)}).toMap
			from2 -> tos2
		})
	}
	def quadToDexNoQuad(a:Int, b:Int, c:Int, d:Int):Tuple4[DexNo, DexNo, DexNo, DexNo] = {
		Tuple4(DexNo.national(a), DexNo.national(b), DexNo.national(c), DexNo.national(d))
	}
	def nationalDexNoSet(a:Int*):Set[DexNo] = {
		a.toSet.map(DexNo.national.apply _)
	}
	def nationalDexNoSeq(a:Int*):Seq[DexNo] = {
		a.toSeq.map(DexNo.national.apply _)
	}

	describe("SeedData") {
		describe("threeEvoChains") {
			it ("When no evolutions, Then Does not report results") {
				val dut = new MySeedData(Map.empty)
				assertResult(Seq.empty)(dut.threeEvoChains)
			}
			it ("When a one-stage evolution exists, Then Does not report results") {
				val dut = new MySeedData(Map(1 -> Seq(2)))
				assertResult(Seq.empty)(dut.threeEvoChains)
			}
			it ("When a two-stage evolution exists, Then Does not report results") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(3)))
				assertResult(Seq.empty)(dut.threeEvoChains)
			}
			it ("When a three-stage evolution exists, Then Does report a results") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(3), 3 -> Seq(4)))
				assertResult(Seq(quadToDexNoQuad(1, 2, 3, 4)))(dut.threeEvoChains)
			}
			it ("When two three-stage evolution exists, Then Does report two results") {
				val dut = new MySeedData(Map(
					1 -> Seq(2), 2 -> Seq(3), 3 -> Seq(4),
					6 -> Seq(7), 7 -> Seq(8), 8 -> Seq(9),
				))
				assertResult(Seq(quadToDexNoQuad(1, 2, 3, 4), quadToDexNoQuad(6, 7, 8, 9)))(dut.threeEvoChains)
			}
			it ("When a split three-stage evolution exists, Then Does report two results") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(3), 3 -> Seq(4, 5)))
				assertResult(Seq(quadToDexNoQuad(1, 2, 3, 4), quadToDexNoQuad(1, 2, 3, 5)))(dut.threeEvoChains)
			}
		}
		describe("finalEvolutions") {
			it ("When no evolutions, then no final evolutions are reported") {
				val dut = new MySeedData(Map.empty)
				for (x <- dut.extantDexNos) {
					assertResult(Seq.empty)(dut.finalEvolutions(x))
				}
			}
			it ("When a single-stage evolution, the evolution is reported as the final evolution of the prevolution") {
				val dut = new MySeedData(Map(1 -> Seq(2)))
				assertResult(nationalDexNoSeq(2))(dut.finalEvolutions(DexNo.national(1)))
			}
			it ("When a multiple-stage evolution, the chain is followed to its conclusion") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(3), 3 -> Seq(4)))
				assertResult(nationalDexNoSeq(4))(dut.finalEvolutions(DexNo.national(1)))
			}
			it ("When a split evolution, both are reported (terminal)") {
				val dut = new MySeedData(Map(1 -> Seq(2, 3)))
				assertResult(nationalDexNoSeq(2, 3))(dut.finalEvolutions(DexNo.national(1)))
			}
			it ("When a split evolution, both are reported (middle)") {
				val dut = new MySeedData(Map(1 -> Seq(2, 3), 2 -> Seq(4), 3 -> Seq(5)))
				assertResult(nationalDexNoSeq(4, 5))(dut.finalEvolutions(DexNo.national(1)))
			}
			it ("Does not infinitely recurse, even when an evolution does") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(2)))
				assertResult(nationalDexNoSeq(2))(dut.finalEvolutions(DexNo.national(1)))
			}
		}
		describe("firstStageMons") {
			it ("when no evos, then all are first stage") {
				val dut = new MySeedData(Map.empty)
				assertResult(dut.extantDexNos.to[Set])(dut.firstStageMons)
			}
			it ("when an evos, then the evolution is not in the firstStageMons list") {
				val dut = new MySeedData(Map(1 -> Seq(2)))
				assertResult(dut.extantDexNos.to[Set] - DexNo.national(2))(dut.firstStageMons)
			}
		}
		describe("families") {
			it ("when no evos, then a mon's family includes only itself") {
				val dut = new MySeedData(Map.empty)
				for (x <- dut.extantDexNos) {
					assertResult(Set(x))(dut.families(x))
				}
			}
			it ("when an evos, then families includes that entry") {
				val dut = new MySeedData(Map(1 -> Seq(2)))
				assertResult(nationalDexNoSet(1, 2))(dut.families(DexNo.national(2)))
			}
			it ("when an multi-stage evos, then families includes that entry") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(3)))
				assertResult(nationalDexNoSet(1, 2, 3))(dut.families(DexNo.national(3)))
			}
			it ("when an merging evos, then families includes both branches") {
				val dut = new MySeedData(Map(1 -> Seq(3), 2 -> Seq(3)))
				assertResult(nationalDexNoSet(1, 2, 3))(dut.families(DexNo.national(3)))
			}
			it ("Combines the past few conditions properly") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(5), 3 -> Seq(4), 4 -> Seq(5)))
				assertResult(nationalDexNoSet(1, 2, 3, 4, 5))(dut.families(DexNo.national(5)))
			}
		}
		describe("prevos") {
			it ("when no evos, then a mon has no prevos") {
				val dut = new MySeedData(Map.empty)
				assertResult(Map.empty)(dut.prevos)
			}
			it ("when an evos, then prevos includes that entry") {
				val dut = new MySeedData(Map(1 -> Seq(2)))
				assertResult(nationalDexNoSet(1))(dut.prevos(DexNo.national(2)))
			}
			it ("when an multi-stage evos, then prevos includes only the immediate prevo") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(3)))
				assertResult(nationalDexNoSet(2))(dut.prevos(DexNo.national(3)))
			}
			it ("when an merging evos, then prevos includes both branches") {
				val dut = new MySeedData(Map(1 -> Seq(3), 2 -> Seq(3)))
				assertResult(nationalDexNoSet(1, 2))(dut.prevos(DexNo.national(3)))
			}
			it ("Combines the past few conditions properly") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(5), 3 -> Seq(4), 4 -> Seq(5)))
				assertResult(nationalDexNoSet(2, 4))(dut.prevos(DexNo.national(5)))
			}
		}
		describe("multiplePrevos") {
			it ("when no evos, then the result set is empty") {
				val dut = new MySeedData(Map.empty)
				assertResult(Set.empty)(dut.multiplePrevos)
			}
			it ("when an evos, then the result set is empty") {
				val dut = new MySeedData(Map(1 -> Seq(2)))
				assertResult(Set.empty)(dut.multiplePrevos)
			}
			it ("when an multi-stage evos, then the final is not added to the result set") {
				val dut = new MySeedData(Map(1 -> Seq(2), 2 -> Seq(3)))
				assertResult(Set.empty)(dut.multiplePrevos)
			}
			it ("when an merging evos, then the evo is added to the result set") {
				val dut = new MySeedData(Map(1 -> Seq(3), 2 -> Seq(3)))
				assertResult(nationalDexNoSet(3))(dut.multiplePrevos)
			}
		}
	}
}
