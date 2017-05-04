package com.rayrobdod.possibleEvolutions

import sbt._
import java.nio.file.Files
import java.util.Date
import java.text.SimpleDateFormat
import com.codecommit.antixml.{Group, Text, Elem, Attributes,
		Node, NamespaceBinding, XMLConvertable, ProcInstr
}
import com.rayrobdod.website.base.constants.xml.{processingInstruction => xmlProcInstr}
import com.rayrobdod.website.base.constants.xhtml.{binding => htmlBinding}
import com.rayrobdod.website.base.constants.xhtml.{doctype => htmlDoctype}

object PageTemplates {
	
	def index(all:ListOfPokemon, prologue:Group[Node]):Group[Node] = {
		Group(xmlProcInstr, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions"))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "style/style.css")),
					Elem(htmlBinding, "script", Attributes("defer" -> "defer", "type" -> "text/javascript", "src" -> "style/tableSort.js"), Group(Text(" ")))
				)),
				Elem(htmlBinding, "body", Attributes(), Group(
					Elem(htmlBinding, "header", Attributes(), Group(
						
					)),
					Elem(htmlBinding, "main", Attributes(), Group(
						Elem(htmlBinding, "h1", Attributes(), Group(Text("Index"))),
						Elem(htmlBinding, "div", Attributes(), prologue),
						pokemonListTable(all.rawdata.tail, Map.empty, all.possibleEvosCount, all.possiblePrevosCount)
					))
				))
			))
		)
	}
	
	def perMonPage(checkno:Int, all:ListOfPokemon):Group[Node] = {
		val checkMon = all.rawdata(checkno)
		val evos = all.possibleEvolutions(checkno)
		val prevos = all.possiblePrevolutions(checkno)
		val realPrevos:Seq[(EvosGame.Value, Int)] = {
			val allEvoPairs:Seq[(Int, EvosGame.Value, Int)] = all.rawdata.flatMap{mon =>
				val a:Seq[(EvosGame.Value, Int)] = mon.evos.toList.flatMap{_._2}
				a.map{x => ((mon.dexNo, x._1, x._2))}
			}
			val relevantEvoPairs = allEvoPairs.filter{_._3 == checkno}
			relevantEvoPairs.map{x => ((x._2, x._1))}
		}
		
		Group(xmlProcInstr, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions - " + checkMon.name))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "style/style.css")),
					Elem("script", Attributes("defer" -> "defer", "type" -> "text/javascript", "src" -> "style/tableSort.js"), Group(Text(" ")))
				)),
				Elem(htmlBinding, "body", Attributes(), Group(
					Elem(htmlBinding, "header", Attributes(), Group(
						
					)),
					Elem(htmlBinding, "main", Attributes(), Group(
						Elem(htmlBinding, "h1", Attributes(), Group(Text(checkMon.name))),
						Elem(htmlBinding, "table", Attributes(), Group(
							tableRow(Seq("Number", checkMon.dexNo.toString)),
							tableRow(Seq("Type1", checkMon.type1)),
							tableRow(Seq("Type2", checkMon.type2)),
							tableRow(Seq("Platinum Type1", checkMon.rpType1)),
							tableRow(Seq("Platinum Type2", checkMon.rpType2)),
							tableRow(Seq("Base Stat Total", checkMon.bst.toString))
						)),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Evos"))),
						Elem(htmlBinding, "div", Attributes(), Group.fromSeq(evos.flatMap{x:(String, Seq[Pokemon]) =>
							val method = x._1
							val naturalBst = all.rawdata(checkMon.naturalEvos(method)).bst
							val realEvos = checkMon.evos(method).map{_._2}.map(all.rawdata)
							
							Seq(
								Elem(htmlBinding, "h3", Attributes(), Group(Text(x._1))),
								Elem(htmlBinding, "div", Attributes(), Group(Text(x._2.size.toString))),
								Elem(htmlBinding, "a", Attributes("href" -> ("http://veekun.com/dex/pokemon/search?type=" + checkMon.type1.toLowerCase +
										"&type=" + checkMon.type2.toLowerCase + "&stat_total=" + (naturalBst * 0.8).intValue + "-" + (naturalBst * 1.2).intValue)), Group(Text("Veekun Version"))),
								pokemonListTable(x._2 ++ realEvos.filterNot{x._2.contains(_)}, checkMon.evos(method), all.possibleEvosCount, all.possiblePrevosCount)
							)
						}.toSeq)),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Prevos"))),
						Elem(htmlBinding, "div", Attributes(), Group(Text(prevos.size.toString))),
						pokemonListTable(prevos ++ realPrevos.map{_._2}.map(all.rawdata).filterNot{prevos.contains(_)}, realPrevos, all.possibleEvosCount, all.possiblePrevosCount)
					))
				))
			))
		)
	}
	
	
	def tableRow(strs:Seq[String]):Node = {
		Elem(htmlBinding, "tr", Attributes(), Group.fromSeq(strs.map{x =>
			Elem(htmlBinding, "td", Attributes(), Group(Text(x)))
		}))
	}
	
	def pokemonListTable(x:Seq[Pokemon], realEvos:Iterable[(EvosGame.Value, Int)], possibleEvosCount:Int => Int, possiblePrevosCount:Int => Int):Node = {
		Elem(htmlBinding, "table", Attributes("class" -> "pokemon-list"), Group(
			Elem(htmlBinding, "thead", Attributes(), Group(
				Elem(htmlBinding, "tr", Attributes(), Group(
					Elem(htmlBinding, "th", Attributes(), Group(Text("DexNo"))),
					Elem(htmlBinding, "th", Attributes(), Group(Text("Name"))),
					Elem(htmlBinding, "th", Attributes(), Group(Text("Type1"))),
					Elem(htmlBinding, "th", Attributes(), Group(Text("Type2"))),
					Elem(htmlBinding, "th", Attributes(), Group(Text("BST"))),
					Elem(htmlBinding, "th", Attributes(), Group(Text("Evos"))),
					Elem(htmlBinding, "th", Attributes(), Group(Text("Prevos")))
				))
			)),
			Elem(htmlBinding, "tbody", Attributes(), Group.fromSeq(
				x.map{(pokemonTableRow(realEvos, possibleEvosCount, possiblePrevosCount) _)}
			))
		))
	}
	
	def pokemonTableRow(realEvos:Iterable[(EvosGame.Value, Int)], possibleEvosCount:Int => Int, possiblePrevosCount:Int => Int)(x:Pokemon):Node = {
		val game = realEvos.filter{_._2 == x.dexNo}.map{_._1}.to[Seq].distinct.map{_.toString}.mkString("", " ", "")
		
		Elem(htmlBinding, "tr", Attributes("data-game" -> game), Group(
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(x.dexNo)), Group(Text(x.dexNo.toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.name), Group(
				Elem(htmlBinding, "a", Attributes("href" -> (x.dexNo + ".html")), Group(Text(x.name)))
			)),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.type1.toLowerCase, "data-type" -> x.type1.toLowerCase), Group(Text(x.type1))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.type2.toLowerCase, "data-type" -> x.type2.toLowerCase), Group(Text(x.type2))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(x.bst)), Group(Text(x.bst.toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(possibleEvosCount(x.dexNo))), Group(Text(possibleEvosCount(x.dexNo).toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(possiblePrevosCount(x.dexNo))), Group(Text(possiblePrevosCount(x.dexNo).toString)))
		))
	}
	
	def padStrWithZeros(x:Int) = ("00000" + x).takeRight(5)
	
}
