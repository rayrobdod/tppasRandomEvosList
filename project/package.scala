package com.rayrobdod

import sbt._
import java.io.File
import java.nio.file.Files
import com.opencsv.{CSVReader, CSVWriter}
import java.nio.charset.StandardCharsets.UTF_8
import com.codecommit.antixml.{Group, Node, NamespaceBinding, ProcInstr}

package object possibleEvolutions {
	
	val xmlProcessingInstruction = ProcInstr("xml", "version=\"1.0\" encoding=\"utf-8\"")
	val htmlNamespace = "http://www.w3.org/1999/xhtml"
	val htmlBinding = NamespaceBinding(htmlNamespace)
	object htmlDoctype extends ProcInstr("DOCTYPE", "html") {
		override val toString = "<!DOCTYPE html>"
	}
	
	def readPrologue(readmemdFile:File):Group[Node] = {
		import com.codecommit.antixml.{Text, Elem, Attributes}
		import scala.collection.JavaConversions._
		val containsLink = """([^\[]*)\[(\w+)\]\(([\w\:\/\.]+)\)(.*)""".r
		
		val emptyParagraph = Elem(htmlBinding, "p")
		
		val lines = Files.readAllLines(readmemdFile.toPath, UTF_8)
		val usedLines = lines.dropWhile{!_.startsWith("#")}.drop(1)
				.takeWhile{!_.startsWith("##")}
				.dropWhile{_ == "\n"}
				.reverse.dropWhile{_ == "\n"}.reverse
		usedLines.foldLeft(Group(emptyParagraph)){(folding, line) => line match {
			case "" => folding :+ emptyParagraph
			case containsLink(before, label, href, after) => {
				folding.init :+ (folding.last addChild Text(" ") addChild Text(before)
						addChild Elem(htmlBinding, "a", Attributes("href" -> href), Group(Text(label)))
						addChild Text(after))
			}
			case _ => {
				folding.init :+ (folding.last addChild Text(" ") addChild Text(line))
			}
		}}
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
	) {
		def exists(implicit config:Configuration.Value):Boolean = config match {
			case Configuration.RandPlat => this.dexNo <= 493
			case Configuration.Gen5 => this.dexNo <= 649
			case Configuration.Gen6 => this.dexNo <= 721
		}
		
		def types(implicit config:Configuration.Value):(String, String) = config match {
			case Configuration.RandPlat => ((rpType1, rpType2))
			case Configuration.Gen6 => ((naturalType1, naturalType2))
			case Configuration.Gen5 => {
				val _1 = (if (naturalType1 == "Fairy") {"Normal"} else {naturalType1})
				val _2 = (if (naturalType2 == "Fairy") {_1} else {naturalType2})
				((_1, _2))
			}
		}
		
		def bst(implicit config:Configuration.Value):Int = config match {
			case Configuration.RandPlat => gen2bst
			case Configuration.Gen5 => gen2bst
			case Configuration.Gen6 => gen6bst
		}
	}
	
	object EvosGame extends Enumeration {
		val Natural = Value(0, "natural")
		val AlphaSapphire = Value(1, "alpha-sapphire")
		val Platinum = Value(2, "platinum")
	}
	
	object MonToMatch extends Enumeration {
		val BaseForm = Value
		val EvolvedForm = Value
	}
	
	object Configuration {
		sealed trait Value { def monToMatch:MonToMatch.Value }
		
		object RandPlat extends Value { val monToMatch = MonToMatch.BaseForm }
		object Gen5 extends Value { val monToMatch = MonToMatch.BaseForm }
		object Gen6 extends Value { val monToMatch = MonToMatch.EvolvedForm }
		
		val values:Set[Configuration.Value] = Set(RandPlat, Gen5, Gen6)
		
		def forGame(game:EvosGame.Value):Configuration.Value = game match {
			case EvosGame.Natural => Gen5
			case EvosGame.AlphaSapphire => Gen6
			case EvosGame.Platinum => RandPlat
		}
	}
}
