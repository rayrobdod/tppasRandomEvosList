package com.rayrobdod.possibleEvolutions

import sbt._
import java.nio.file.Files
import java.util.Date
import java.text.SimpleDateFormat
import com.codecommit.antixml.{Group, Text, Elem, Attributes,
		Node, NamespaceBinding, XMLConvertable, ProcInstr
}

object PageTemplates {
	private val HTML_NAMESPACE = "http://www.w3.org/1999/xhtml"
	private val htmlBinding = NamespaceBinding(HTML_NAMESPACE)
	private val xmlProcInstr = ProcInstr("xml", "version=\"1.0\" encoding=\"utf-8\"")
	private object htmlDoctype extends ProcInstr("DOCTYPE", "html") {
		override def toString = "<!DOCTYPE html>"
	}
	/** Assumes no namespaces */
	private class ExplicitCloseElem(b:String,c:Attributes) extends ProcInstr("FAKE", b) {
		override def toString = {
			type A[X] = String
			"<" + b + " " + c.flatMap{x => x._1.name + "='" + x._2 + "' "}.to[A] + "></" + b + ">"
		}
	}
	
	// TODO: ew, globals
	var evosCacheReadDir:File = new File(".")
	lazy val evosCache:Map[Int, (Int, Int)] = {
		val x = getAllPokemon(evosCacheReadDir)
		x.map{p:Pokemon =>
			val key = p.dexNo
			val evos = findPossibleEvolutions(key, x).map{_._2}.flatten.toSet.size
			val prevos = findPossiblePrevolutions(key, x).size
			((key, ((evos, prevos))))
		}.toMap
	}
	
	def index(all:Seq[Pokemon], prologue:Group[Node]):Group[Node] = {
		Group(xmlProcInstr, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions"))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "style/style.css")),
					new ExplicitCloseElem("script", Attributes("defer" -> "defer", "type" -> "text/javascript", "src" -> "style/tableSort.js"))
				)),
				Elem(htmlBinding, "body", Attributes(), Group(
					Elem(htmlBinding, "header", Attributes(), Group(
						
					)),
					Elem(htmlBinding, "main", Attributes(), Group(
						Elem(htmlBinding, "h1", Attributes(), Group(Text("Index"))),
						Elem(htmlBinding, "div", Attributes(), prologue),
						pokemonListTable(all)
					))
				))
			))
		)
	}
	
	def perMonPage(checkno:Int, all:Seq[Pokemon]):Group[Node] = {
		val checkMon = all(checkno)
		val evos = findPossibleEvolutions(checkno, all)
		val prevos = findPossiblePrevolutions(checkno, all)
		val realPrevos:Seq[(EvosGame.Value, Int)] = {
			val allEvoPairs:Seq[(Int, EvosGame.Value, Int)] = all.flatMap{mon =>
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
					new ExplicitCloseElem("script", Attributes("defer" -> "defer", "type" -> "text/javascript", "src" -> "style/tableSort.js"))
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
							val naturalBst = all(checkMon.naturalEvos(method)).bst
							val realEvos = checkMon.evos(method).map{_._2}.map(all)
							
							Seq(
								Elem(htmlBinding, "h3", Attributes(), Group(Text(x._1))),
								Elem(htmlBinding, "div", Attributes(), Group(Text(x._2.size.toString))),
								Elem(htmlBinding, "a", Attributes("href" -> ("http://veekun.com/dex/pokemon/search?type=" + checkMon.type1.toLowerCase +
										"&type=" + checkMon.type2.toLowerCase + "&stat_total=" + (naturalBst * 0.8).intValue + "-" + (naturalBst * 1.2).intValue)), Group(Text("Veekun Version"))),
								pokemonListTable(x._2 ++ realEvos.filterNot{x._2.contains(_)}, checkMon.evos(method))
							)
						}.toSeq)),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Prevos"))),
						Elem(htmlBinding, "div", Attributes(), Group(Text(prevos.size.toString))),
						pokemonListTable(prevos ++ realPrevos.map{_._2}.map(all).filterNot{prevos.contains(_)}, realPrevos)
					))
				))
			))
		)
	}
	
	def perGamePage(game:EvosGame.Value, all:Seq[Pokemon]):Group[Node] = {
		
		val evolutionList:Seq[(Pokemon, String, Pokemon)] = all.flatMap{from =>
			from.evos.mapValues{x => x.find{_._1 == game}.map{_._2}.getOrElse(0)}.to[Seq]
					.map{case (method, toNo) => ((from, method, all(toNo)))}
		}
		
		Group(xmlProcInstr, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions - " + game.toString))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "style/style.css")),
					new ExplicitCloseElem("script", Attributes("defer" -> "defer", "type" -> "text/javascript", "src" -> "style/tableSort.js"))
				)),
				Elem(htmlBinding, "body", Attributes(), Group(
					Elem(htmlBinding, "header", Attributes(), Group(
						
					)),
					Elem(htmlBinding, "main", Attributes(), Group(
						Elem(htmlBinding, "h1", Attributes(), Group(Text(game.toString))),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Evolutions"))),
						Elem(htmlBinding, "table", Attributes("class" -> "evolution-list"), Group(
							Elem(htmlBinding, "thead", Attributes(), Group(
								Elem(htmlBinding, "tr", Attributes(), Group(
									Elem(htmlBinding, "th", Attributes(), Group(Text("From DexNo"))),
									Elem(htmlBinding, "th", Attributes(), Group(Text("From Name"))),
									Elem(htmlBinding, "th", Attributes(), Group(Text("Method"))),
									Elem(htmlBinding, "th", Attributes(), Group(Text("To DexNo"))),
									Elem(htmlBinding, "th", Attributes(), Group(Text("To Name")))
								))
							)),
							Elem(htmlBinding, "tbody", Attributes(), Group.fromSeq(
								 evolutionList.map{case (from, method, to) =>
									Elem(htmlBinding, "tr", Attributes(), Group(
										Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(from.dexNo)), Group(Text(from.dexNo.toString))),
										Elem(htmlBinding, "td", Attributes("data-sort" -> from.name), Group(
											Elem(htmlBinding, "a", Attributes("href" -> (from.dexNo + ".html")), Group(Text(from.name)))
										)),
										Elem(htmlBinding, "td", Attributes("data-sort" -> method), Group(Text(method))),
										Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(to.dexNo)), Group(Text(to.dexNo.toString))),
										Elem(htmlBinding, "td", Attributes("data-sort" -> to.name), Group(
											Elem(htmlBinding, "a", Attributes("href" -> (to.dexNo + ".html")), Group(Text(to.name)))
										))
									))
								}
							))
						)),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Pokémon that nothing evolves into"))),
						pokemonListTable(
							all.filterNot(evolutionList.map{_._3}.toSet)
						),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Pokémon that multiple things evolves into"))),
						pokemonListTable(
							evolutionList.foldLeft(Map.empty[Pokemon, Seq[Pokemon]]){(folding, next) =>
								val (from, _, to) = next
								folding + (to -> (folding.getOrElse(to, Seq.empty) :+ from))
							}.filter{_._2.size >= 2}.to[Seq].map{_._1}.sortBy{_.dexNo}
						)
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
	
	def pokemonListTable(x:Seq[Pokemon], realEvos:Iterable[(EvosGame.Value, Int)] = Map.empty):Node = {
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
				x.map{(pokemonTableRow(realEvos) _)}
			))
		))
	}
	
	def pokemonTableRow(realEvos:Iterable[(EvosGame.Value, Int)])(x:Pokemon):Node = {
		val game = realEvos.filter{_._2 == x.dexNo}.map{_._1}.to[Seq].distinct.map{_.toString}.mkString("", " ", "")
		
		Elem(htmlBinding, "tr", Attributes("data-game" -> game), Group(
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(x.dexNo)), Group(Text(x.dexNo.toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.name), Group(
				Elem(htmlBinding, "a", Attributes("href" -> (x.dexNo + ".html")), Group(Text(x.name)))
			)),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.type1.toLowerCase, "data-type" -> x.type1.toLowerCase), Group(Text(x.type1))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.type2.toLowerCase, "data-type" -> x.type2.toLowerCase), Group(Text(x.type2))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(x.bst)), Group(Text(x.bst.toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(evosCache(x.dexNo)._1)), Group(Text(evosCache(x.dexNo)._1.toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(evosCache(x.dexNo)._2)), Group(Text(evosCache(x.dexNo)._2.toString)))
		))
	}
	
	def padStrWithZeros(x:Int) = ("00000" + x).takeRight(5)
	
}
