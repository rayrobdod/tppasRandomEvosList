package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.{Seq, IndexedSeq}

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
	def national(x:Int) = new DexNo(x, "")
	def alola(x:Int) = new DexNo(x, "A")
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
				case NATIONAL_RANGE(low, high) => DexNo.NationalDexNoRange(low.toInt, high.toInt)
				case x => Seq(DexNo.valueOf(x))
			}
		}.reduce{_ ++ _}
	}
	
	/** A range from min to max, both sides inclusive, where min and max represent national dex numbers */
	final case class NationalDexNoRange(min:Int, max:Int) extends IndexedSeq[DexNo] {
		
		override def apply(idx:Int):DexNo = {
			if (idx < 0 || idx + min > max) {
				throw new IndexOutOfBoundsException();
			} else {
				DexNo.national(idx + min);
			}
		}
		override def length:Int = max - min + 1
		override def contains[A1 >: DexNo](x:A1):Boolean = x match {
			case DexNo(value, "") => (min <= value && value <= max)
			case _ => false
		}
	}
	val alolanDexNos:Seq[DexNo] = Seq(
		DexNo.alola(19), DexNo.alola(20),
		DexNo.alola(26),
		DexNo.alola(27), DexNo.alola(28),
		DexNo.alola(37), DexNo.alola(38),
		DexNo.alola(50), DexNo.alola(51),
		DexNo.alola(52), DexNo.alola(53),
		DexNo.alola(74), DexNo.alola(75), DexNo.alola(76),
		DexNo.alola(88), DexNo.alola(89),
		DexNo.alola(103), DexNo.alola(105)
	)
}
