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
	
	lazy val evosCache:Map[Int, (Int, Int)] = {
		val x = getAllPokemon
		x.map{p:Pokemon =>
			val key = p.dexNo
			val evos = findPossibleEvolutions(key, x).flatten.size
			val prevos = findPossiblePrevolutions(key, x).size
			((key, ((evos, prevos))))
		}.toMap
	}
	
	def index(all:Seq[Pokemon]):Group[Node] = {
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
						pokemonListTable(all)
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
							tableRow(Seq("Base Stat Total", checkMon.bst.toString))
						)),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Evos"))),
						Elem(htmlBinding, "div", Attributes(), Group.fromSeq(evos.zip(evosBst).flatMap{x => Seq(
							Elem(htmlBinding, "br"),
							Elem(htmlBinding, "div", Attributes(), Group(Text(x._1.size.toString))),
							Elem(htmlBinding, "a", Attributes("href" -> ("http://veekun.com/dex/pokemon/search?type=" + checkMon.type1.toLowerCase +
									"&type=" + checkMon.type2.toLowerCase + "&stat_total=" + (x._2 * 0.8).intValue + "-" + (x._2 * 1.2).intValue)), Group(Text("Veekun Version"))),
							pokemonListTable(x._1)
						)})),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Prevos"))),
						Elem(htmlBinding, "div", Attributes(), Group(Text(prevos.size.toString))),
						pokemonListTable(prevos)
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
	
	def pokemonListTable(x:Seq[Pokemon]):Node = {
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
				x.map{pokemonTableRow _}
			))
		))
	}
	
	def pokemonTableRow(x:Pokemon):Node = {
		Elem(htmlBinding, "tr", Attributes(), Group(
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
