package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.{BitSet, Seq, IndexedSeq}

/**
 * Represents a Pokemon's unique identifier/primary key.
 */
final class DexNo private (private val number:Int, private val variant:String) extends Ordered[DexNo] {
	override def toString:String = number.toString + variant
	def toStringPadded:String = ("00000" + number).takeRight(5) + variant

	override def compare(rhs:DexNo):Int = {
		val a = this.number compare rhs.number
		val b = this.variant compare rhs.variant
		if (a == 0) {b} else {a}
	}
	override def hashCode:Int = number + variant.hashCode * 1001
	override def equals(rhs:Any):Boolean = rhs match {
		case DexNo(rhsValue, rhsVar) => rhsValue == this.number && rhsVar == this.variant
		case _ => false
	}
}

object DexNo {
	/** Creates a DexNo with the value of a national dex number */
	val national = new VariantApplication("")
	val alola = new VariantApplication("A")
	val galar = new VariantApplication("G")
	def duskRockruff = new DexNo(744, "DUSK")
	def antiqueSinistea = new DexNo(854, "RARE")
	def waterUrshifu = new DexNo(892, "WATER")
	val fused = new VariantApplication("FUSED")
	private def unapply(x:Any):Option[(Int, String)] = x match {
		case x2:DexNo => Option((x2.number, x2.variant))
		case _ => None
	}

	def valueOf(x:String):DexNo = {
		val FORMAT = """(\d+)([A-Z]*)""".r
		x match {
			case FORMAT(a, b) => new DexNo(a.toInt, b)
		}
	}
	def seqValueOf(xs:String):Seq[DexNo] = {
		xs.split(',').map{x =>
			val NATIONAL_RANGE = """(\d+)\-(\d+)""".r

			x match {
				case "Galardex" => DexNoSets.Galar
				case "Galardlcdex" => DexNoSets.GalarDlc
				case "AlolanForms" => DexNoSets.alolanForms
				case "GalaranForms" => DexNoSets.galaranForms
				case "GalaranDlcForms" => DexNoSets.galaranDlcForms
				case "FusedCrystal" => DexNoSets.FusedCrystal
				case NATIONAL_RANGE(low, high) => DexNoSets.NationalRange(low.toInt, high.toInt)
				case x => Seq(DexNo.valueOf(x))
			}
		}.reduce{_ ++ _}
	}

	final class VariantApplication(variant:String) {
		def apply(x:Int):DexNo = new DexNo(x, variant)
		def unapply(x:Any):Option[Int] = x match {
			case DexNo(num, `variant`) => Option(num)
			case _ => None
		}
	}
}

object DexNoSets {
	private[this] val GalarNums = BitSet(
		1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 25, 26, 35, 36, 37, 38, 43, 44, 45, 50, 51, 52, 53,
		58, 59, 66, 67, 68, 77, 78, 83, 90, 91, 92, 93, 94, 95, 98, 99, 106, 107, 109, 110, 111,
		112, 118, 119, 122, 129, 130, 131, 132, 133, 134, 135, 136, 143, 150, 151, 163, 164, 170,
		171, 172, 173, 175, 176, 177, 178, 182, 185, 194, 195, 196, 197, 202, 208, 211, 213, 215,
		220, 221, 222, 223, 224, 225, 226, 236, 237, 246, 247, 248, 251, 263, 264, 270, 271, 272,
		273, 274, 275, 278, 279, 280, 281, 282, 290, 291, 292, 302, 303, 309, 310, 315, 320, 321,
		324, 328, 329, 330, 337, 338, 339, 340, 341, 342, 343, 344, 349, 350, 355, 356, 360, 361,
		362, 385, 406, 407, 415, 416, 420, 421, 422, 423, 425, 426, 434, 435, 436, 437, 438, 439,
		446, 447, 448, 449, 450, 451, 452, 453, 454, 458, 459, 460, 461, 464, 468, 470, 471, 473,
		475, 477, 478, 479, 509, 510, 517, 518, 519, 520, 521, 524, 525, 526, 527, 528, 529, 530,
		532, 533, 534, 535, 536, 537, 538, 539, 546, 547, 550, 554, 555, 556, 557, 558, 559, 560,
		561, 562, 563, 568, 569, 572, 573, 574, 575, 576, 577, 578, 579, 582, 583, 584, 588, 589,
		592, 593, 595, 596, 597, 598, 599, 600, 601, 605, 606, 607, 608, 609, 610, 611, 612, 613,
		614, 616, 617, 618, 622, 623, 624, 625, 627, 628, 629, 630, 631, 632, 633, 634, 635, 638,
		639, 640, 643, 644, 646, 647, 659, 660, 674, 675, 677, 678, 679, 680, 681, 682, 683, 684,
		685, 686, 687, 688, 689, 694, 695, 700, 701, 704, 705, 706, 708, 709, 710, 711, 712, 713,
		714, 715, 722, 723, 724, 725, 726, 727, 728, 729, 730, 736, 737, 738, 742, 743, 746, 747,
		748, 749, 750, 751, 752, 755, 756, 757, 758, 759, 760, 761, 762, 763, 765, 766, 767, 768,
		771, 772, 773, 776, 777, 778, 780, 781, 782, 783, 784, 789, 790, 791, 792, 800, 802, 807,
		808, 809, 810, 811, 812, 813, 814, 815, 816, 817, 818, 819, 820, 821, 822, 823, 824, 825,
		826, 827, 828, 829, 830, 831, 832, 833, 834, 835, 836, 837, 838, 839, 840, 841, 842, 843,
		844, 845, 846, 847, 848, 849, 850, 851, 852, 853, 854, 855, 856, 857, 858, 859, 860, 861,
		862, 863, 864, 865, 866, 867, 868, 869, 870, 871, 872, 873, 874, 875, 876, 877, 878, 879,
		880, 881, 882, 883, 884, 885, 886, 887, 888, 889, 890)

	private[this] val GalarDlcNums = GalarNums ++ BitSet(
		27, 28, 29, 30, 31, 32, 33, 34, 39, 40, 41, 42, 54, 55, 60, 61, 62, 63, 64, 65, 72, 73, 79,
		80, 81, 82, 102, 103, 104, 105, 108, 113, 114, 115, 116, 117, 120, 121, 123, 124, 125, 126,
		127, 128, 137, 138, 139, 140, 141, 142, 144, 145, 146, 147, 148, 149, 169, 174, 183, 184,
		186, 199, 206, 212, 214, 227, 230, 233, 238, 239, 240, 241, 242, 293, 294, 295, 298, 304,
		305, 306, 318, 319, 333, 334, 345, 346, 347, 348, 359, 363, 364, 365, 369, 371, 372, 373,
		374, 375, 376, 377, 378, 379, 403, 404, 405, 427, 428, 440, 442, 443, 444, 445, 462, 463,
		465, 466, 467, 474, 506, 507, 508, 531, 543, 544, 545, 548, 549, 551, 552, 553, 564, 565,
		566, 567, 570, 571, 587, 590, 591, 615, 619, 620, 621, 626, 636, 637, 661, 662, 663, 690,
		691, 692, 693, 696, 697, 698, 699, 702, 703, 707, 744, 745, 753, 754, 764, 769, 770, 891,
		892, 893, 894, 895, 896, 897, 898)

	private[this] val AlolanFormNums = BitSet(19, 20, 26, 27, 28, 37, 38, 50, 51, 52, 53, 74, 75,
		76, 88, 89, 103, 105)
	private[this] val GalaranFormNums = BitSet(52, 77, 78, 83, 110, 122, 222, 263, 264, 554, 555,
		562, 618)
	private[this] val GalaranDlcFormNums = BitSet(79, 80, 144, 145, 146, 199)

	val alolanForms:Seq[DexNo] = AlolanFormNums.to[Seq].map(DexNo.alola.apply _)
	val galaranForms:Seq[DexNo] = GalaranFormNums.to[Seq].map(DexNo.galar.apply _)
	val galaranDlcForms:Seq[DexNo] = GalaranDlcFormNums.to[Seq].map(DexNo.galar.apply _)

	val Gen1:Seq[DexNo] = NationalRange(1, 151)
	val Gen2:Seq[DexNo] = NationalRange(1, 251)
	val Gen3:Seq[DexNo] = NationalRange(1, 386)
	val Gen4:Seq[DexNo] = NationalRange(1, 493)
	val Gen5:Seq[DexNo] = NationalRange(1, 649)
	val Gen6:Seq[DexNo] = NationalRange(1, 721)
	val Gen7:Seq[DexNo] = NationalRange(1, 802) ++ alolanForms
	val Gen7Ultra:Seq[DexNo] = NationalRange(1, 807) ++ alolanForms :+ DexNo.duskRockruff
	val Galar:Seq[DexNo] = GalarNums.to[Seq].map(DexNo.national.apply _) ++
		(GalarNums & AlolanFormNums).to[Seq].map(DexNo.alola.apply _) ++
		galaranForms :+ DexNo.antiqueSinistea
	val GalarDlc:Seq[DexNo] = GalarDlcNums.to[Seq].map(DexNo.national.apply _) ++
		(GalarDlcNums & AlolanFormNums).to[Seq].map(DexNo.alola.apply _) ++
		galaranForms ++ galaranDlcForms :+ DexNo.duskRockruff :+ DexNo.antiqueSinistea :+
		DexNo.waterUrshifu
	val Gen8:Seq[DexNo] = NationalRange(1, 890) ++ alolanForms ++ galaranForms :+
		DexNo.duskRockruff :+ DexNo.antiqueSinistea
	val Gen8Dlc:Seq[DexNo] = NationalRange(1, 898) ++ alolanForms ++ galaranForms ++
		galaranDlcForms :+ DexNo.duskRockruff :+ DexNo.antiqueSinistea :+ DexNo.waterUrshifu
	val FusedCrystal:Seq[DexNo] = (1 to 200).map(DexNo.fused.apply _) ++ Seq(DexNo.national(201)) ++
		(202 to 251).map(DexNo.fused.apply _)

	/** A range from min to max, both sides inclusive, where min and max represent national dex numbers */
	final case class NationalRange(min:Int, max:Int) extends IndexedSeq[DexNo] {
		override def apply(idx:Int):DexNo = {
			if (idx < 0 || idx + min > max) {
				throw new IndexOutOfBoundsException();
			} else {
				DexNo.national(idx + min);
			}
		}
		override def length:Int = max - min + 1
		override def contains[A1 >: DexNo](x:A1):Boolean = x match {
			case DexNo.national(value) => (min <= value && value <= max)
			case _ => false
		}
	}
}
