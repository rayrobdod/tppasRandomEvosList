package com.rayrobdod

import java.io.File
import java.nio.file.Files
import com.opencsv.{CSVReader, CSVWriter}
import java.nio.charset.StandardCharsets.UTF_8

package object possibleEvolutions {
	
	val htmlDoctype = "<!DOCTYPE html>"
	
	def readPrologue(readmemdFile:File):scalatags.Text.Frag = {
		import scalatags.Text.StringFrag
		import scalatags.Text.all.stringAttr
		import scala.collection.JavaConversions._
		val containsLink = """([^\[]*)\[(\w+)\]\(([\w\:\/\.]+)\)(.*)""".r
		
		val emptyParagraph = scalatags.Text.tags.p
		
		val lines = Files.readAllLines(readmemdFile.toPath, UTF_8)
		val usedLines = lines.dropWhile{!_.startsWith("#")}.drop(1)
				.takeWhile{!_.startsWith("##")}
				.dropWhile{_ == "\n"}
				.reverse.dropWhile{_ == "\n"}.reverse
		scalatags.Text.all.frag(usedLines.foldLeft(Seq(emptyParagraph)){(folding, line) => line match {
			case "" if folding.last == emptyParagraph => folding
			case "" => folding :+ emptyParagraph
			case containsLink(before, label, href, after) => {
				folding.init :+ (folding.last(StringFrag(" "), StringFrag(before),
						scalatags.Text.tags.a(scalatags.Text.attrs.href := href)(StringFrag(label)),
						StringFrag(after)))
			}
			case _ => {
				folding.init :+ (folding.last(StringFrag(" "), StringFrag(line)))
			}
		}}:_*)
	}
	
	def appendRow(csvFile:File, row:Seq[String]):Unit = {
		import scala.collection.JavaConversions.collectionAsScalaIterable
		val inData:Seq[Seq[String]] = {
			val inReader = new CSVReader(Files.newBufferedReader(csvFile.toPath, UTF_8))
			val inData = inReader.readAll.to[Seq].map{_.to[Seq]}
			inReader.close()
			inData
		}
		val outData:Seq[Seq[String]] = inData.zip(row).map{case (in, r) => in :+ r}
		
		val outWriter = new CSVWriter(Files.newBufferedWriter(csvFile.toPath, UTF_8))
		outData.foreach{line => outWriter.writeNext(line.to[Array])}
		outWriter.close()
	}
	
}

package possibleEvolutions {
	
	final case class DexNo(private val value:Int) extends Ordered[DexNo] {
		override def toString:String = value.toString
		def toStringPadded:String = ("00000" + value).takeRight(5) 
		
		override def compare(rhs:DexNo) = this.value compare rhs.value
		
		def <=(x:Int) = this.value <= x
	}
	object DexNo {
		val missing:DexNo = DexNo(0)
		val undef:DexNo = DexNo(-1)
	}
	
	final class Pokemon(
		val dexNo:DexNo = DexNo.undef,
		val name:String = "",
		naturalType1:String = "",
		naturalType2:String = "",
		rpType1:String = "",
		rpType2:String = "",
		gen1bst:Int = -1,
		gen2bst:Int = -1,
		gen6bst:Int = -1,
		gen7bst:Int = -1
		, val expGrowth:String = ""
	) {
		def exists(implicit config:EvosGame.Value):Boolean = { 
			val maxNo = config match {
				case EvosGame.Platinum => 493
				case EvosGame.White2 => 649
				case EvosGame.AlphaSapphire => 721
				case EvosGame.Natural => 802
			}
			DexNo.missing < this.dexNo && this.dexNo <= maxNo 
		}
		
		// Create the tuples only once, reducing GC churn
		private[this] val rpTypes = ((rpType1, rpType2))
		private[this] val natTypes = ((naturalType1, naturalType2))
		private[this] val noFairyTypes = {
			val _1 = (if (naturalType1 == "Fairy") {"Normal"} else {naturalType1})
			val _2 = (if (naturalType2 == "Fairy") {_1} else {naturalType2})
			((_1, _2))
		}
		
		def types(implicit config:EvosGame.Value):(String, String) = config match {
			case EvosGame.Platinum => rpTypes
			case EvosGame.AlphaSapphire | EvosGame.Natural => natTypes
			case EvosGame.White2 => noFairyTypes
		}
		
		def bst(implicit config:EvosGame.Value):Int = config match {
			case EvosGame.Natural => gen7bst
			case EvosGame.Platinum => gen2bst
			case EvosGame.White2 => gen2bst
			case EvosGame.AlphaSapphire => gen6bst
		}
	}
	
	object EvosGame {
		sealed trait Value {
			def id:Int
			def name:String
			def monToMatch:MonToMatch.Value
			def expGroupMustMatch:Boolean
			def showSeedData:Boolean
			
			override def toString = this.name
		}
		
		object Natural extends Value {
			override def id:Int = 0
			override def name:String = "natural"
			override def monToMatch:MonToMatch.Value = MonToMatch.BaseForm
			override def expGroupMustMatch:Boolean = true
			override def showSeedData:Boolean = true
		}
		
		object AlphaSapphire extends Value {
			override def id:Int = 1
			override def name:String = "alpha-sapphire"
			override def monToMatch:MonToMatch.Value = MonToMatch.EvolvedForm
			override def expGroupMustMatch:Boolean = false
			override def showSeedData:Boolean = true
		}
		
		object Platinum extends Value {
			override def id:Int = 2
			override def name:String = "platinum"
			override def monToMatch:MonToMatch.Value = MonToMatch.BaseForm
			override def expGroupMustMatch:Boolean = true
			override def showSeedData:Boolean = true
		}
		
		object White2 extends Value {
			override def id:Int = 3
			override def name:String = "white2"
			override def monToMatch:MonToMatch.Value = MonToMatch.Neither
			override def expGroupMustMatch:Boolean = true
			override def showSeedData:Boolean = false
		}
		
		def values:Seq[Value] = Seq(Natural, AlphaSapphire, Platinum, White2)
	}
	
	object MonToMatch extends Enumeration {
		val BaseForm = Value
		val EvolvedForm = Value
		val Neither = Value
	}
	
}
