package com.rayrobdod.possibleEvolutions

import scalatags.Text.tags.{attr => _, frag => _, _}
import scalatags.Text.attrs.{tag => _, modifier => _, _}
import scalatags.Text.implicits._

object PageTemplates {
	private[this] val main = tag("main")
	private[this] val defer = attr("defer")
	private[this] val dataGame = data("game")
	private[this] val dataSort = data("sort")
	private[this] val dataType = data("type")
	
	
	def index(prologue:scalatags.Text.Frag):scalatags.Text.Frag = {
		html(lang := "en-US")(
			  head(
				  title := ("Possible Evolutions")
				, link(rel := "stylesheet", href := "style/style.css")
			  )
			, body(
				main(
					  h1("Index")
					, div(prologue)
					, h2("Games List")
					, ul(`class` := "gamesList")(modifier(
						EvosGame.values.to[Seq].tail.map{game =>
							val name = game.toString
							li(dataGame := name)(
								a(href := (name + "/index.html"))(name)
							)
						}:_*
					  ))
				)
			  )
		)
	}
	
	def perMonPage(checkno:DexNo, all:ListOfPokemon)(implicit config:EvosGame.Value):scalatags.Text.Frag = {
		val checkMon = all.getPokemon(checkno)
		val evos = all.possibleEvolutions(checkno)
		val prevos = all.possiblePrevolutions(checkno)
		val realPrevos = all.prevos(checkno)
		val prevos2 = prevos.flatMap{mon => all.possiblePrevolutions(mon.dexNo)}.distinct
		val realPrevos2 = realPrevos.flatMap{case (game, dexNo) => all.prevos(dexNo).filter(_._1 == game)}.distinct
		
		html(lang := "en.US")(
			  head(
				  title := s"Possible Evolutions - $config - ${checkMon.name}"
				, link(rel := "stylesheet", href := "../style/style.css")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/sectionCollapse.js")(" ")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/tableSort.js")(" ")
			  )
			, body(
				  header(
					  a(href := "../index.html")("Index")
					, " > "
					, a(href := "index.html")("Game")
				  )
				, main(
					  h1(checkMon.name)
					, table(`class` := "general-info")(
						  monInfoTableRow("Number", checkMon.dexNo.toString)
						, monInfoTableRowType("Type1", checkMon.types._1)
						, monInfoTableRowType("Type2", checkMon.types._2)
						, monInfoTableRow("Base Stat Total", checkMon.bst.toString)
						, monInfoTableRow("Experience Group", checkMon.expGrowth)
					  )
					, h2("Possible Evos")
					, div(evos.flatMap{case (method:String, possibleEvos:Seq[Pokemon]) =>
						val naturalEvo = all.getPokemon(all.naturalEvos(checkMon.dexNo)(method))
						val naturalBst = naturalEvo.bst
						val realEvos = all.evolutions(checkMon.dexNo)(method)
						
						val veekunSearchLink = {
							val growthRate = checkMon.expGrowth match {
								case "Slow" => "1250000"
								case "Medium Fast" => "1000000"
								case "Fast" => "800000"
								case "Medium Slow" => "1059860"
								case "Erratic" => "600000"
								case "Fluctuating" => "1640000"
							}
							val types = config.monToMatch match {
								case MonToMatch.Neither => ""
								case MonToMatch.BaseForm => {
									val (type1, type2) = checkMon.types
									s"&type=${type1.toLowerCase}&type=${type2.toLowerCase}"
								}
								case MonToMatch.EvolvedForm => {
									val (type1, type2) = naturalEvo.types
									s"&type=${type1.toLowerCase}&type=${type2.toLowerCase}"
								}
							}
							
							(
								"http://veekun.com/dex/pokemon/search?" +
								s"stat_total=${naturalBst * 5 / 6}-${naturalBst * 6 / 5}" +
								(if (config.expGroupMustMatch) {s"&growth_rate=$growthRate"} else {""}) ++
								types
							)
						}
						
						Seq(
							  h3(method)
							, div(a(href := veekunSearchLink)("Veekun search with same parameters"))
							, monPredictionSection(possibleEvos, realEvos, all.possibleEvosCount, all.possiblePrevosCount, all.getPokemon)
						)
					  }.to[Seq]:_*)
					, h2("Possible Prevos")
					, monPredictionSection(prevos, realPrevos, all.possibleEvosCount, all.possiblePrevosCount, all.getPokemon)
					, h2("Possible Prevos^2")
					, monPredictionSection(prevos2, realPrevos2, all.possibleEvosCount, all.possiblePrevosCount, all.getPokemon)
				)
			)
		)
	}
	
	def perGamePage(game:EvosGame.Value, all:ListOfPokemon):scalatags.Text.Frag = {
		implicit val config = game
		
		val evolutionList:Seq[(Pokemon, String, Pokemon)] = {
			for (
				(prevoDexno, prevoDexnoInfo) <- all.evolutions;
				(method, methodInfo) <- prevoDexnoInfo;
				(mGame, evoDexno) <- methodInfo if (game == mGame)
			) yield {
				((all.getPokemon(prevoDexno), method, all.getPokemon(evoDexno)))
			}
		}.to[Seq].sortBy{_._1.dexNo}
		
		html(lang := "en-US")(
			  head(
				  title := s"Possible Evolutions - $game"
				, link(rel := "stylesheet", href := "../style/style.css")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/sectionCollapse.js")(" ")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/tableSort.js")(" ")
			  )
			, body(
				  header(
				  	a(href := "../index.html")("Back to Index")
				  )
				, main(
				  	  h1(game.toString)
				  	, h2("Pokémon List")
				  	, pokemonListTable(all.allPokemon.tail.filter{_.exists}, Map.empty, all.possibleEvosCount, all.possiblePrevosCount)
				  	)(if (game.showSeedData) {frag(
				  		  h2("Evolutions")
				  		, table(`class` := "evolution-list")(
				  			  thead(
				  				tr(
									  th("From DexNo")
									, th("From Name")
									, th("Method")
									, th("To DexNo")
									, th("To Name")
								)
							  )
							, tbody(
								evolutionList.map{case (from, method, to) =>
									tr(
										  td(dataSort := from.dexNo.toStringPadded)(from.dexNo.toString)
										, td(dataSort := from.name)(
											a(href := s"${from.dexNo}.html")(from.name)
										  )
										, td(dataSort := method)(method)
										, td(dataSort := to.dexNo.toStringPadded)(to.dexNo.toString)
										, td(dataSort := to.name)(
											a(href := s"${to.dexNo}.html")(to.name)
										  )
									)
								}:_*
							  )
				  		  )
						, h2("Pokémon that nothing evolves into")
						, pokemonListTable(
							all.allPokemon.filterNot(evolutionList.map{_._3}.toSet).filter{_.exists}
							, Seq.empty, all.possibleEvosCount, all.possiblePrevosCount
						  )
						, h2("Pokémon that multiple things evolve into")
						, pokemonListTable(
							evolutionList.foldLeft(Map.empty[Pokemon, Seq[Pokemon]]){(folding, next) =>
								val (from, _, to) = next
								folding + (to -> (folding.getOrElse(to, Seq.empty) :+ from))
							}.filter{_._2.size >= 2}.to[Seq].map{_._1}.filter{_.exists}.sortBy{_.dexNo}
							, Seq.empty, all.possibleEvosCount, all.possiblePrevosCount
						  )
						, h2("Pokémon whose evo isn't predicted")
						, pokemonListTable(
							(for (
								(prevoNo, prevonodata) <- all.evolutions;
								(method, methoddata) <- prevonodata;
								realEvoNo <- methoddata.get(game)
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
						, h2("Pokémon whose evo matches vanilla")
						, pokemonListTable(
							(for (
								(prevoNo, prevonodata) <- all.evolutions;
								(method, evolutions) <- prevonodata
							) yield {
								if (evolutions.get(EvosGame.Natural) == evolutions.get(game)) {
									Seq(prevoNo)
								} else {
									Seq.empty
								}
							}).flatten.to[Seq].distinct.map{all.getPokemon _}
							, Seq.empty, all.possibleEvosCount, all.possiblePrevosCount
						  )
						, h2("4 stage evolution chains")
						, ul({
							val singleEvoChain:Map[DexNo, DexNo] = (for (
								(prevoNo, prevonodata) <- all.evolutions.to[Seq];
								(_, methoddata) <- prevonodata;
								evoNo <- methoddata.get(game)
							) yield {
								(prevoNo, evoNo)
							}).toMap
							
							val threeEvoChain:Seq[(DexNo, DexNo, DexNo, DexNo)] = for (
								(first, second) <- singleEvoChain.to[Seq];
								third <- singleEvoChain.get(second);
								fourth <- singleEvoChain.get(third)
							) yield ((first, second, third, fourth))
							
							threeEvoChain.map{case (p1,p2,p3,p4) => 
								li(
									  a(href := s"${p1}.html")(s"${p1} ${all.getPokemon(p1).name}")
									, " → "
									, a(href := s"${p2}.html")(s"${p2} ${all.getPokemon(p2).name}")
									, " → "
									, a(href := s"${p3}.html")(s"${p3} ${all.getPokemon(p3).name}")
									, " → "
									, a(href := s"${p4}.html")(s"${p4} ${all.getPokemon(p4).name}")
								)
							}
						  }:_*)
					)} else {frag("")}
				  )
			  )
		)
	}
	
	private[this] def monPredictionSection(
			  possible:Seq[Pokemon]
			, observed:Iterable[(EvosGame.Value, DexNo)]
			, possibleEvosCount:DexNo => Int
			, possiblePrevosCount:DexNo => Int
			, resolveDexNo:DexNo => Pokemon
			)(implicit config:EvosGame.Value
	):scalatags.Text.Frag = frag(
		  div(s"Number of candidates: ${possible.size}")
		, (if (! config.showSeedData) {frag("")} else {h4("Observed")})
		, (if (! config.showSeedData) {frag("")} else {
			val observedThisGame = observed.filter{_._1 == config}.map{_._2}.map(resolveDexNo).to[Seq]
			pokemonListTable(observedThisGame, observed, possibleEvosCount, possiblePrevosCount)
		  })
		, h4("Candidates")
		, pokemonListTable(possible, observed, possibleEvosCount, possiblePrevosCount)
	)
	
	private[this] def monInfoTableRow(m_th:String, m_td:String):scalatags.Text.TypedTag[String] = {
		tr(th(m_th), td(m_td))
	}
	
	private[this] def monInfoTableRowType(m_th:String, m_td:String):scalatags.Text.TypedTag[String] = {
		tr(
			  th(m_th)
			, td(dataType := m_td.toLowerCase)(m_td)
		)
	}
	
	private[this] def pokemonListTable(
			  x:Iterable[Pokemon]
			, realEvos:Iterable[(EvosGame.Value, DexNo)]
			, possibleEvosCount:DexNo => Int
			, possiblePrevosCount:DexNo => Int
			)(implicit config:EvosGame.Value
	):scalatags.Text.TypedTag[String] = {
		table(`class` := "pokemon-list")(
			  thead(
				tr(
					  th("DexNo")
					, th("Name")
					, th("Type1")
					, th("Type2")
					, th("BST")
					, th("Exp Group")
					, th("Evos")
					, th("Prevos")
				)
			  )
			, tbody((
				x
					.to[Seq]
					.sorted
					.distinct
					.map{(pokemonTableRow(realEvos, possibleEvosCount, possiblePrevosCount) _)}
			):_*)
		)
	}
	
	private[this] def pokemonTableRow(realEvos:Iterable[(EvosGame.Value, DexNo)], possibleEvosCount:DexNo => Int, possiblePrevosCount:DexNo => Int)(x:Pokemon)(implicit config:EvosGame.Value):scalatags.Text.TypedTag[String] = {
		val game = realEvos.filter{_._2 == x.dexNo}.map{_._1}.to[Seq].distinct.sortBy{_.id}.mkString("", " ", "")
		
		tr(dataGame := game)(
			  td(data("sort") := x.dexNo.toStringPadded)(x.dexNo.toString)
			, td(data("sort") := x.name)(
				a(href := s"${x.dexNo}.html")(x.name)
			  )
			, td(data("type") := x.types._1.toLowerCase)(x.types._1)
			, td(data("type") := x.types._2.toLowerCase)(x.types._2) 
			, td(data("sort") := padStrWithZeros(x.bst))(x.bst.toString)
			, td(data("sort") := x.expGrowth)(x.expGrowth)
			, td(data("sort") := padStrWithZeros(possibleEvosCount(x.dexNo)))(possibleEvosCount(x.dexNo).toString)
			, td(data("sort") := padStrWithZeros(possiblePrevosCount(x.dexNo)))(possiblePrevosCount(x.dexNo).toString)
		)
	}
	
	private[this] def padStrWithZeros(x:Int):String = ("00000" + x).takeRight(5)
	
}
