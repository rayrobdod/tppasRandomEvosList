package com.rayrobdod

import sbt._
import java.io.File
import java.nio.file.Files
import com.opencsv.{CSVReader, CSVWriter}
import java.nio.charset.StandardCharsets.UTF_8

package object possibleEvolutions {
	
	def getAllPokemon(dir:File):Seq[Pokemon] = {
			findAlphaEvolutions(dir,
			findNaturalEvolutions(dir,
			 readListOfPokemon(dir)))
	}
	
	private def readListOfPokemon(dir:File):Seq[Pokemon] = {
		val inFile = new File(dir, "listOfPokemon.csv")
		val inReader = new CSVReader(Files.newBufferedReader(inFile.toPath, UTF_8))
		val inData = inReader.readAll.toArray.toSeq.map{_ match {
			case Array(dexNo:String, name:String, type1:String, type2:String, bst:String) => {
				Pokemon(dexNo.toInt, name, type1, type2, bst.toInt, Nil)
			}
		}}
		inReader.close()
		inData
	}
	
	private def findNaturalEvolutions(dir:File, in:Seq[Pokemon]):Seq[Pokemon] = {
		val evoFile = new File(dir, "naturalEvolutions.csv")
		val evoReader = new CSVReader(Files.newBufferedReader(evoFile.toPath, UTF_8))
		val evoData = evoReader.readAll.toArray.toSeq.map{_ match {
			case Array(inNo:String, inName:String, outNo:String, outName:String) => {
				((inNo.toInt, outNo.toInt))
			}
		}}
		evoReader.close()
		
		in.map{x:Pokemon =>
			val resultNos = evoData.filter{_._1 == x.dexNo}.map{_._2}
			val resultMons = resultNos.map{y => in.find{_.dexNo == y}}.flatten.toSeq
			x.copy(naturalEvoNo = resultMons.map{_.dexNo})
		}
	}
	
	private def findAlphaEvolutions(dir:File, in:Seq[Pokemon]):Seq[Pokemon] = {
		val evoFile = new File(dir, "alphaSapphireEvolutions.csv")
		val evoReader = new CSVReader(Files.newBufferedReader(evoFile.toPath, UTF_8))
		val evoData = evoReader.readAll.toArray.toSeq.map{_ match {
			case Array(inNo:String, inName:String, outNo:String, outName:String) => {
				((inNo.toInt, outNo.toInt))
			}
		}}
		evoReader.close()
		
		in.map{x:Pokemon =>
			val resultNos = evoData.filter{_._1 == x.dexNo}.map{_._2}
			val resultMons = resultNos.map{y => in.find{_.dexNo == y}}.flatten.toSeq
			x.copy(randomizedEvoNo = resultMons.map{_.dexNo})
		}
	}
	
	def findPossibleEvolutions(checkNo:Int, all:Seq[Pokemon]):Seq[Seq[Pokemon]] = {
		val mon = all.find{_.dexNo == checkNo}.get
		
		def typesMatch(a1:String, a2:String, b1:String, b2:String) = {
			a1 == b1 || a1 == b2 || a2 == b1 || a2 == b2
		}
		
		// https://github.com/kwsch/pk3DS/blob/master/pk3DS/Subforms/Evolution.cs#L202
		mon.naturalEvoNo.map{all}.map{y => all.filter{x => (x.bst * 6 / 5 > y.bst) && (y.bst > x.bst * 5 / 6) && typesMatch(y.type1, y.type2, x.type1, x.type2)}}
	}
	
	def findPokemonCapableOfEvolvingIntoItself(all:Seq[Pokemon]):Seq[Pokemon] = {
		all
			.map{x => ((x, findPossibleEvolutions(x.dexNo, all).flatten))}
			.filter({(x:Pokemon,y:Seq[Pokemon]) => y.map{_.dexNo}.contains(x.dexNo)}.tupled)
			.map{_._1}
	}
	
	def findPossiblePrevolutions(checkNo:Int, all:Seq[Pokemon]):Seq[Pokemon] = {
		all
			.map{x => ((x, findPossibleEvolutions(x.dexNo, all).flatten))}
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
		naturalEvoNo:Seq[Int] = Nil,
		randomizedEvoNo:Seq[Int] = Nil
	)
	
	object EvosType extends Enumeration {
		val Natural = Value(0, "natural")
		val AlphaSapphire = Value(1, "alpha-sapphire")
	}
}

