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
		import scala.collection.JavaConversions._
		val HTML_NAMESPACE = "http://www.w3.org/1999/xhtml"
		val htmlBinding = NamespaceBinding(HTML_NAMESPACE)
		val containsLink = """([^\[]*)\[(\w+)\]\(([\w\:\/\.]+)\)(.*)""".r
		
		val emptyParagraph = Elem(htmlBinding, "p")
		
		val lines = Files.readAllLines(readmemdFile.toPath)
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
	
	def getAllPokemon(dir:File):Seq[Pokemon] = {
			findEvolutions(dir,
			readListOfPokemon(dir))
	}
	
	private def readListOfPokemon(dir:File):Seq[Pokemon] = {
		val inFile = new File(dir, "listOfPokemon.csv")
		val inReader = new CSVReader(Files.newBufferedReader(inFile.toPath, UTF_8))
		val inData = inReader.readAll.toArray.toSeq.map{_ match {
			case Array(dexNo:String, name:String, type1:String, type2:String, bst:String) => {
				Pokemon(dexNo.toInt, name, type1, type2, bst.toInt, Map.empty)
			}
		}}
		inReader.close()
		inData
	}
	
	private def findEvolutions(dir:File, in:Seq[Pokemon]):Seq[Pokemon] = {
		def readEvoDataFile(fileName:String, game:EvosGame.Value):Seq[(Int, Int, String, EvosGame.Value)] = {
			val f = new File(dir, fileName)
			val r = new CSVReader(Files.newBufferedReader(f.toPath, UTF_8))
			val data = r.readAll.toArray.toSeq.map{_ match {
				case Array(inNo:String, inName:String, outNo:String, outName:String, method:String) => {
					((inNo.toInt, outNo.toInt, method, game))
				}
			}}
			r.close()
			data
		}
		val natural = readEvoDataFile("naturalEvolutions.csv", EvosGame.Natural)
		val alphaSapphire = readEvoDataFile("alphaSapphireEvolutions.csv", EvosGame.AlphaSapphire)
		val summed:Seq[(Int, Int, String, EvosGame.Value)] = natural ++ alphaSapphire 
		
		
		in.map{mon:Pokemon =>
			val methods = natural.filter{_._1 == mon.dexNo}.map{_._3}
			val evos:Map[String, Map[EvosGame.Value, Int]] = methods.map{method:String =>
				((method,
					summed.filter{x => x._1 == mon.dexNo && x._3 == method}
							.map{x => ((x._4, x._2))}
							.toMap
				))
			}.toMap
			mon.copy(evos = evos)
		}
	}
	
	def findPossibleEvolutions(checkNo:Int, all:Seq[Pokemon]):Map[String, Seq[Pokemon]] = {
		val checkMon = all.find{_.dexNo == checkNo}.get
		
		def typesMatch(a1:String, a2:String, b1:String, b2:String) = {
			a1 == b1 || a1 == b2 || a2 == b1 || a2 == b2
		}
		
		val naturalEvoNos:Map[String, Int] = checkMon.naturalEvos
		val naturalEvoMons:Map[String, Pokemon] = naturalEvoNos.mapValues(all)
		
		// https://github.com/kwsch/pk3DS/blob/master/pk3DS/Subforms/Evolution.cs#L202
		naturalEvoMons.mapValues{y => all.filter{x => (x.bst * 6 / 5 > y.bst) && (y.bst > x.bst * 5 / 6) && typesMatch(y.type1, y.type2, x.type1, x.type2)}}
	}
	
	def findPokemonCapableOfEvolvingIntoItself(all:Seq[Pokemon]):Seq[Pokemon] = {
		all
			.map{x => ((x, findPossibleEvolutions(x.dexNo, all).map{_._2}.flatten.toSeq))}
			.filter({(x:Pokemon,y:Seq[Pokemon]) => y.map{_.dexNo}.contains(x.dexNo)}.tupled)
			.map{_._1}
	}
	
	def findPossiblePrevolutions(checkNo:Int, all:Seq[Pokemon]):Seq[Pokemon] = {
		all
			.map{x => ((x, findPossibleEvolutions(x.dexNo, all).map{_._2}.flatten.toSeq))}
			.filter({(x:Pokemon,y:Seq[Pokemon]) => y.map{_.dexNo}.contains(checkNo)}.tupled)
			.map{_._1}
	}
	
	
}

package possibleEvolutions {

	case class Pokemon(
		dexNo:Int = -1,
		name:String = "",
		type1:String = "",
		type2:String = "",
		bst:Int = -1,
		evos:Map[String, Map[EvosGame.Value, Int]] = Map.empty
	) {
		def naturalEvos:Map[String, Int] = evos.mapValues{x => x.find{_._1 == EvosGame.Natural}.map{_._2}.getOrElse(0)}
	}
	
	object EvosGame extends Enumeration {
		val Natural = Value(0, "natural")
		val AlphaSapphire = Value(1, "alpha-sapphire")
	}
}

