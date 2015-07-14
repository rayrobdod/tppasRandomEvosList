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
	
	def index(all:Seq[Pokemon]):Group[Node] = {
		Group(xmlProcInstr, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions"))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "style/style.css"))
				)),
				Elem(htmlBinding, "body", Attributes(), Group(
					Elem(htmlBinding, "header", Attributes(), Group(
						
					)),
					Elem(htmlBinding, "main", Attributes(), Group(
						Elem(htmlBinding, "h1", Attributes(), Group(Text("Index"))),
						Elem(htmlBinding, "table", Attributes(), Group.fromSeq(
							Elem(htmlBinding, "tr", Attributes(), Group(
								Elem(htmlBinding, "th", Attributes(), Group(Text("Name"))),
								Elem(htmlBinding, "th", Attributes(), Group(Text("Type1"))),
								Elem(htmlBinding, "th", Attributes(), Group(Text("Type2"))),
								Elem(htmlBinding, "th", Attributes(), Group(Text("BST")))
							)) +:
							all.map{pokemonTableRow _}
						))
					))
				))
			))
		)
	}
	
	def perMonPage(checkno:Int, all:Seq[Pokemon]):Group[Node] = {
		val checkMon = all(checkno)
		val evosBst = checkMon.evoBst.toSet.toSeq
		val evos = findPossibleEvolutions(checkno, all)
		val prevos = findPossiblePrevolutions(checkno, all)
		
		Group(xmlProcInstr, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions - " + checkMon.name))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "style/style.css"))
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
							tableRow(Seq("Base Stat Total", checkMon.bst.toString))
						)),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Evos"))),
						Elem(htmlBinding, "div", Attributes(), Group.fromSeq(evos.zip(evosBst).flatMap{x => Seq(
							Elem(htmlBinding, "br"),
							Elem(htmlBinding, "div", Attributes(), Group(Text(x._1.size.toString))),
							Elem(htmlBinding, "a", Attributes("href" -> ("http://veekun.com/dex/pokemon/search?type=" + checkMon.type1.toLowerCase +
									"&type=" + checkMon.type2.toLowerCase + "&stat_total=" + (x._2 * 0.8).intValue + "-" + (x._2 * 1.2).intValue)), Group(Text("Veekun Version"))),
							Elem(htmlBinding, "table", Attributes(), Group.fromSeq(
								Elem(htmlBinding, "tr", Attributes(), Group(
									Elem(htmlBinding, "th", Attributes(), Group(Text("Name"))),
									Elem(htmlBinding, "th", Attributes(), Group(Text("Type1"))),
									Elem(htmlBinding, "th", Attributes(), Group(Text("Type2"))),
									Elem(htmlBinding, "th", Attributes(), Group(Text("BST")))
								)) +:
								x._1.map{pokemonTableRow _}
							))
						)})),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Prevos"))),
						Elem(htmlBinding, "div", Attributes(), Group(Text(prevos.size.toString))),
						Elem(htmlBinding, "table", Attributes(), Group.fromSeq(
							Elem(htmlBinding, "tr", Attributes(), Group(
								Elem(htmlBinding, "th", Attributes(), Group(Text("Name"))),
								Elem(htmlBinding, "th", Attributes(), Group(Text("Type1"))),
								Elem(htmlBinding, "th", Attributes(), Group(Text("Type2"))),
								Elem(htmlBinding, "th", Attributes(), Group(Text("BST")))
							)) +:
							prevos.map{pokemonTableRow _}
						))
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
	
	def pokemonTableRow(x:Pokemon):Node = {
		Elem(htmlBinding, "tr", Attributes(), Group(
			Elem(htmlBinding, "td", Attributes(), Group(
				Elem(htmlBinding, "a", Attributes("href" -> (x.dexNo + ".html")), Group(Text(x.name)))
			)),
			Elem(htmlBinding, "td", Attributes(), Group(Text(x.type1))),
			Elem(htmlBinding, "td", Attributes(), Group(Text(x.type2))),
			Elem(htmlBinding, "td", Attributes(), Group(Text(x.bst.toString))),
			Elem(htmlBinding, "td", Attributes(), Group(Text(findPossiblePrevolutions(x.dexNo, getAllPokemon).size.toString)))
		))
	}
	
}
