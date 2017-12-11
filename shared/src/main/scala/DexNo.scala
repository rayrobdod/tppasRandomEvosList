package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.IndexedSeq

/**
 * Represents a Pokemon's unique identifier/primary key.
 */
final class DexNo private (private val value:Int) extends Ordered[DexNo] {
	override def toString:String = value.toString
	def toStringPadded:String = ("00000" + value).takeRight(5) 
	
	override def compare(rhs:DexNo):Int = this.value compare rhs.value
	override def hashCode:Int = value
	override def equals(rhs:Any):Boolean = rhs match {
		case DexNo(rhsValue) => rhsValue == this.value
		case _ => false
	}
}

object DexNo {
	/** Creates a DexNo with the value of a national dex number */
	def national(x:Int) = new DexNo(x)
	private def unapply(x:Any):Option[Int] = x match {
		case x2:DexNo => Option(x2.value)
		case _ => None
	}
	
	def valueOf(x:String):DexNo = DexNo.national(x.toInt)
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
				new DexNo(idx + min);
			}
		}
		override def length:Int = max - min + 1
		override def contains[A1 >: DexNo](x:A1):Boolean = x match {
			case DexNo(value) => (min <= value && value <= max)
			case _ => false
		}
	}
}
