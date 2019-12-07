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
	def duskRockruff = new DexNo(744, "DUSK")
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
	private[this] val AlolanFormNums = BitSet(19, 20, 26, 27, 28, 37, 38, 50, 51, 52, 53, 74, 75,
		76, 88, 89, 103, 105)

	val alolan:Seq[DexNo] = AlolanFormNums.to[Seq].map(DexNo.alola.apply _)

	val Gen1:Seq[DexNo] = NationalRange(1, 151)
	val Gen2:Seq[DexNo] = NationalRange(1, 251)
	val Gen3:Seq[DexNo] = NationalRange(1, 386)
	val Gen4:Seq[DexNo] = NationalRange(1, 493)
	val Gen5:Seq[DexNo] = NationalRange(1, 649)
	val Gen6:Seq[DexNo] = NationalRange(1, 721)
	val Gen7:Seq[DexNo] = NationalRange(1, 802) ++ alolan
	val Gen7Ultra:Seq[DexNo] = NationalRange(1, 807) ++ alolan :+ DexNo.duskRockruff

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
