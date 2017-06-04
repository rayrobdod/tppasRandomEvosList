package com.rayrobdod.possibleEvolutions

import sbt._
import java.nio.file.Files
import java.util.Date
import java.text.SimpleDateFormat
import com.codecommit.antixml.{Group, Text, Elem, Attributes,
		Node, NamespaceBinding, XMLConvertable, ProcInstr
}

object PageTemplates {
	
	def index(prologue:Group[Node]):Group[Node] = {
		Group(xmlProcessingInstruction, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions"))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "style/style.css"))
				)),
				Elem(htmlBinding, "body", Attributes(), Group(
					Elem(htmlBinding, "main", Attributes(), Group(
						Elem(htmlBinding, "h1", Attributes(), Group(Text("Index"))),
						Elem(htmlBinding, "div", Attributes(), prologue),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Games List"))),
						Elem(htmlBinding, "ul", Attributes("class" -> "gamesList"), Group.fromSeq(EvosGame.values.to[Seq].tail.map{game =>
							val name = game.toString
							Elem(htmlBinding, "li", Attributes("data-game" -> name), Group(
								Elem(htmlBinding, "a", Attributes("href" -> (name + "/index.html")), Group(Text(name)))
							))
						}))
					))
				))
			))
		)
	}
	
	def perMonPage(checkno:DexNo, all:ListOfPokemon)(implicit config:Configuration.Value):Group[Node] = {
		val checkMon = all.getPokemon(checkno)
		val evos = all.possibleEvolutions(checkno)
		val prevos = all.possiblePrevolutions(checkno)
		val realPrevos = all.prevos(checkno)
		
		Group(xmlProcessingInstruction, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions - " + checkMon.name))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "../style/style.css")),
					Elem(htmlBinding, "script", Attributes("defer" -> "defer", "type" -> "text/javascript", "src" -> "../style/tableSort.js"), Group(Text(" ")))
				)),
				Elem(htmlBinding, "body", Attributes(), Group(
					Elem(htmlBinding, "header", Attributes(), Group(
						Elem(htmlBinding, "a", Attributes("href" -> "../index.html"), Group(Text("Index"))),
						Text(" > "),
						Elem(htmlBinding, "a", Attributes("href" -> "index.html"), Group(Text("Game")))
					)),
					Elem(htmlBinding, "main", Attributes(), Group(
						Elem(htmlBinding, "h1", Attributes(), Group(Text(checkMon.name))),
						Elem(htmlBinding, "table", Attributes("class" -> "general-info"), Group(
							monInfoTableRow("Number", checkMon.dexNo.toString),
							monInfoTableRowType("Type1", checkMon.types._1),
							monInfoTableRowType("Type2", checkMon.types._2),
							monInfoTableRow("Base Stat Total", checkMon.bst.toString)
							, monInfoTableRow("Experience Group", checkMon.expGrowth)
						)),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Evos"))),
						Elem(htmlBinding, "div", Attributes(), Group.fromSeq(evos.flatMap{case (method:String, possibleEvos:Seq[Pokemon]) =>
							val naturalBst = all.getPokemon(all.naturalEvos(checkMon.dexNo)(method)).bst
							val realEvos = all.evolutions(checkMon.dexNo)(method).map{_._2}.map(all.getPokemon _)
							
							Seq(
								Elem(htmlBinding, "h3", Attributes(), Group(Text(method))),
								Elem(htmlBinding, "div", Attributes(), Group(Text(possibleEvos.size.toString))),
								Elem(htmlBinding, "a", Attributes("href" -> ("http://veekun.com/dex/pokemon/search?type=" + checkMon.types._1.toLowerCase +
										"&type=" + checkMon.types._2.toLowerCase + "&stat_total=" + (naturalBst * 0.8).intValue + "-" + (naturalBst * 1.2).intValue)), Group(Text("Veekun Version"))),
								pokemonListTable(possibleEvos ++ realEvos.filterNot{possibleEvos.contains(_)}, all.evolutions(checkMon.dexNo)(method), all.possibleEvosCount, all.possiblePrevosCount)
							)
						}.toSeq)),
						Elem(htmlBinding, "h2", Attributes(), Group(Text("Possible Prevos"))),
						Elem(htmlBinding, "div", Attributes(), Group(Text(prevos.size.toString))),
						pokemonListTable(
							  prevos ++ realPrevos.map{_._2}.map(all.getPokemon _).filterNot{prevos.contains(_)}
							, realPrevos, all.possibleEvosCount, all.possiblePrevosCount
						)
					))
				))
			))
		)
	}
	
	def perGamePage(game:EvosGame.Value, all:ListOfPokemon):Group[Node] = {
		implicit val config = Configuration.forGame(game)
		val showSeedData = EvosGame.White2 != game
		
		val evolutionList:Seq[(Pokemon, String, Pokemon)] = {
			for (
				(prevoDexno, prevoDexnoInfo) <- all.evolutions;
				(method, methodInfo) <- prevoDexnoInfo;
				(mGame, evoDexno) <- methodInfo if (game == mGame)
			) yield {
				((all.getPokemon(prevoDexno), method, all.getPokemon(evoDexno)))
			}
		}.to[Seq].sortBy{_._1.dexNo}
		
		
		Group(xmlProcessingInstruction, Text("\n"), htmlDoctype, Text("\n"),
			Elem(htmlBinding, "html", Attributes("lang" -> "en-US"), Group(
				Elem(htmlBinding, "head", Attributes(), Group(
					Elem(htmlBinding, "title", Attributes(), Group(Text("Possible Evolutions - " + game.toString))),
					Elem(htmlBinding, "meta", Attributes("http-equiv" -> "content-type", "content" -> "application/xhtml+xml")),
					Elem(htmlBinding, "link", Attributes("rel" -> "stylesheet", "href" -> "../style/style.css")),
					Elem(htmlBinding, "script", Attributes("defer" -> "defer", "type" -> "text/javascript", "src" -> "../style/sectionCollapse.js"), Group(Text(" "))),
					Elem(htmlBinding, "script", Attributes("defer" -> "defer", "type" -> "text/javascript", "src" -> "../style/tableSort.js"), Group(Text(" ")))
				)),
				Elem(htmlBinding, "body", Attributes(), Group(
					Elem(htmlBinding, "header", Attributes(), Group(
						Elem(htmlBinding, "a", Attributes("href" -> "../index.html"), Group(Text("Back to Index")))
					)),
					Elem(htmlBinding, "main", Attributes(), 
						Group(
							Elem(htmlBinding, "h1", Attributes(), Group(Text(game.toString))),
							Elem(htmlBinding, "h2", Attributes(), Group(Text("Pokémon List"))),
							pokemonListTable(all.allPokemon.tail.filter{_.exists}, Map.empty, all.possibleEvosCount, all.possiblePrevosCount)
						) ++ (
							if (showSeedData) { Group(
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
												Elem(htmlBinding, "td", Attributes("data-sort" -> from.dexNo.toStringPadded), Group(Text(from.dexNo.toString))),
												Elem(htmlBinding, "td", Attributes("data-sort" -> from.name), Group(
													Elem(htmlBinding, "a", Attributes("href" -> (from.dexNo + ".html")), Group(Text(from.name)))
												)),
												Elem(htmlBinding, "td", Attributes("data-sort" -> method), Group(Text(method))),
												Elem(htmlBinding, "td", Attributes("data-sort" -> to.dexNo.toStringPadded), Group(Text(to.dexNo.toString))),
												Elem(htmlBinding, "td", Attributes("data-sort" -> to.name), Group(
													Elem(htmlBinding, "a", Attributes("href" -> (to.dexNo + ".html")), Group(Text(to.name)))
												))
											))
										}
									))
								)),
								Elem(htmlBinding, "h2", Attributes(), Group(Text("Pokémon that nothing evolves into"))),
								pokemonListTable(
									all.allPokemon.filterNot(evolutionList.map{_._3}.toSet).filter{_.exists}
									, Seq.empty, all.possibleEvosCount, all.possiblePrevosCount
								),
								Elem(htmlBinding, "h2", Attributes(), Group(Text("Pokémon that multiple things evolve into"))),
								pokemonListTable(
									evolutionList.foldLeft(Map.empty[Pokemon, Seq[Pokemon]]){(folding, next) =>
										val (from, _, to) = next
										folding + (to -> (folding.getOrElse(to, Seq.empty) :+ from))
									}.filter{_._2.size >= 2}.to[Seq].map{_._1}.filter{_.exists}.sortBy{_.dexNo}
									, Seq.empty, all.possibleEvosCount, all.possiblePrevosCount
								),
								Elem(htmlBinding, "h2", Attributes(), Group(Text("Pokémon whose evo isn't predicted"))),
								pokemonListTable(
									(for (
										(prevoNo, prevonodata) <- all.evolutions;
										(method, methoddata) <- prevonodata;
										(game2, realEvoNo) <- methoddata if game2 == game
									) yield {
										val isPredicted = all.possibleEvolutions(prevoNo)(config)(method).map{_.dexNo} contains realEvoNo
										
										if (isPredicted) {
											Seq.empty
										} else {
											Seq(prevoNo)
										}
									}).flatten.to[Seq].distinct.map{all.getPokemon _}
									, Seq.empty, all.possibleEvosCount, all.possiblePrevosCount
								)
							) } else {
								Group.empty
							}
						)
					)
				))
			))
		)
	}
	
	
	private[this] def monInfoTableRow(th:String, td:String):Node = {
		Elem(htmlBinding, "tr", Attributes(), Group(
			Elem(htmlBinding, "th", Attributes(), Group(Text(th))),
			Elem(htmlBinding, "td", Attributes(), Group(Text(td)))
		))
	}
	
	private[this] def monInfoTableRowType(th:String, td:String):Node = {
		Elem(htmlBinding, "tr", Attributes(), Group(
			Elem(htmlBinding, "th", Attributes(), Group(Text(th))),
			Elem(htmlBinding, "td", Attributes("data-type" -> td.toLowerCase), Group(Text(td)))
		))
	}
	
	def pokemonListTable(x:Seq[Pokemon], realEvos:Iterable[(EvosGame.Value, DexNo)], possibleEvosCount:DexNo => Int, possiblePrevosCount:DexNo => Int)(implicit config:Configuration.Value):Node = {
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
				x
					.sortBy{_.dexNo}
					.distinct
					.map{(pokemonTableRow(realEvos, possibleEvosCount, possiblePrevosCount) _)}
			))
		))
	}
	
	def pokemonTableRow(realEvos:Iterable[(EvosGame.Value, DexNo)], possibleEvosCount:DexNo => Int, possiblePrevosCount:DexNo => Int)(x:Pokemon)(implicit config:Configuration.Value):Node = {
		val game = realEvos.filter{_._2 == x.dexNo}.map{_._1}.to[Seq].distinct.sortBy{_.id}.map{_.toString}.mkString("", " ", "")
		
		Elem(htmlBinding, "tr", Attributes("data-game" -> game), Group(
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.dexNo.toStringPadded), Group(Text(x.dexNo.toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.name), Group(
				Elem(htmlBinding, "a", Attributes("href" -> (x.dexNo + ".html")), Group(Text(x.name)))
			)),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.types._1.toLowerCase, "data-type" -> x.types._1.toLowerCase), Group(Text(x.types._1))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> x.types._2.toLowerCase, "data-type" -> x.types._2.toLowerCase), Group(Text(x.types._2))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(x.bst)), Group(Text(x.bst.toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(possibleEvosCount(x.dexNo))), Group(Text(possibleEvosCount(x.dexNo).toString))),
			Elem(htmlBinding, "td", Attributes("data-sort" -> padStrWithZeros(possiblePrevosCount(x.dexNo))), Group(Text(possiblePrevosCount(x.dexNo).toString)))
		))
	}
	
	def padStrWithZeros(x:Int) = ("00000" + x).takeRight(5)
	
}
