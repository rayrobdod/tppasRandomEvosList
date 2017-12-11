package com.rayrobdod.possibleEvolutions

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.Builder

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
	
	private val maxPlusOneInt:Int = 808
	implicit def mapCanBuildFrom[V]:CanBuildFrom[Map[DexNo, V], (DexNo, V), DexNoMap[V]] = new DexNoMapCanBuildFrom[V]
	
	/**
	 * A Map with DexNo keys taking advantage of the following assumptions:
	 * - DexNos are all small, nonnegative integers
	 */
	final class DexNoMap[+V] private[DexNo] (backing:Array[Option[V]])
			extends scala.collection.immutable.Map[DexNo, V]
			with scala.collection.immutable.MapLike[DexNo, V, DexNoMap[V]] {
		
		override def stringPrefix:String = "DexNoMap"
		override def empty:DexNoMap[V] = new DexNoMap(new Array[Option[V]](0))
		override def +[V1 >: V](kv:(DexNo, V1)):DexNoMap[V1] = {
			val newLength = backing.length max (kv._1.value + 1)
			val backing2 = new Array[Option[V1]](newLength)
			Array.copy(backing, 0, backing2, 0, backing.length)
			(backing.length until newLength).foreach{i => backing2(i) = None}
			backing2(kv._1.value) = Some(kv._2)
			new DexNoMap(backing2)
		}
		override def -(key:DexNo):DexNoMap[V] = {
			if (key.value < backing.length) {
				val backing2 = new Array[Option[V]](backing.length)
				Array.copy(backing, 0, backing2, 0, backing.length)
				backing2(key.value) = None
				new DexNoMap(backing2)
			} else {
				this
			}
		}
		override def get(key:DexNo):Option[V] = backing(key.value)
		override def iterator:Iterator[(DexNo, V)] = {
			new Iterator[(DexNo, V)]{
				/** points at the index of the thing returned by the next call of `next` */
				private var current:Int = -1
				this.advance()
				
				def hasNext:Boolean = {
					current < backing.length
				}
				def next():(DexNo, V) = {
					val retval = ((new DexNo(current), backing(current).get))
					this.advance()
					retval
				}
				private def advance():Unit = {
					current = current + 1
					while (current < backing.length && backing(current) == None) {
						current = current + 1
					}
				}
			}
		}
		
		// Things required for this to be a sorted set
		// def ordering: Ordering[DexNo] = implicitly[Ordering[DexNo]]
		// def rangeImpl(from:Option[DexNo], until:Option[DexNo]):DexNoMap[V] = ???
	}
	final class DexNoMapBuilder[V] extends Builder[(DexNo, V), DexNoMap[V]] {
		private[this] var backing = Array.fill[Option[V]](DexNo.maxPlusOneInt)(None)
		
		override def +=(kv:(DexNo, V)):DexNoMapBuilder.this.type = {backing(kv._1.value) = Option(kv._2); this}
		override def clear():Unit = {backing = Array.fill[Option[V]](DexNo.maxPlusOneInt)(None)}
		override def result():DexNoMap[V] = new DexNoMap(backing)
		
		/** Set every value that is currently unassigned to `v` */
		def withDefault(v:V):Unit = {
			(0 until DexNo.maxPlusOneInt).foreach{idx =>
				if (backing(idx) == None) {backing(idx) = Some(v)}
			}
		}
	}
	final class DexNoMapCanBuildFrom[V] extends CanBuildFrom[Map[DexNo, V], (DexNo, V), DexNoMap[V]] {
		def apply:Builder[(DexNo, V), DexNoMap[V]] = new DexNoMapBuilder
		def apply(from:Map[DexNo,V]):Builder[(DexNo, V), DexNoMap[V]] = new DexNoMapBuilder
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
