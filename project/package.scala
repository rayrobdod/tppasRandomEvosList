package com.rayrobdod

import sbt._
import java.io.File
import java.nio.file.Files
import com.opencsv.{CSVReader, CSVWriter}
import java.nio.charset.StandardCharsets.UTF_8
import com.codecommit.antixml.{Group, Node}

package object possibleEvolutions {
	
	def readPrologue(readmemdFile:File):Group[Node] = {
		import com.codecommit.antixml.{Group, Text, Elem, Attributes,
				Node, NamespaceBinding, XMLConvertable, ProcInstr
		}
		import com.rayrobdod.website.base.constants.xhtml.{binding => htmlBinding}
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
	
	final case class Pokemon(
		dexNo:Int = -1,
		name:String = "",
		type1:String = "",
		type2:String = "",
		rpType1:String = "",
		rpType2:String = "",
		bst:Int = -1,
		evos:Map[String, Map[EvosGame.Value, Int]] = Map.empty
	) {
		def naturalEvos:Map[String, Int] = evos.mapValues{x => x.find{_._1 == EvosGame.Natural}.map{_._2}.getOrElse(0)}
	}
	
	object EvosGame extends Enumeration {
		val Natural = Value(0, "natural")
		val AlphaSapphire = Value(1, "alpha-sapphire")
		val Platinum = Value(2, "platinum")
	}
}

