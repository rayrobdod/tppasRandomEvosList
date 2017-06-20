package com.rayrobdod.possibleEvolutions

import scalatags.Text.tags.{attr => _, frag => _, _}
import scalatags.Text.attrs.{tag => _, modifier => _, title => _, _}
import scalatags.Text.implicits._
import com.rayrobdod.possibleEvolutions.DexNo.mapCanBuildFrom

object PageTemplates {
	private[this] val main = tag("main")
	private[this] val defer = attr("defer")
	private[this] val colgroupSpan = attr("span")
	private[this] val dataGame = data("game")
	private[this] val dataSort = data("sort")
	private[this] val dataType = data("type")
	private[this] val title = tag("title")
	
	
	def index(prologue:scalatags.Text.Frag):scalatags.Text.Frag = {
		html(lang := "en-US")(
			  head(
				  title("Possible Evolutions")
				, link(rel := "stylesheet", href := "style/style.css")
			  )
			, body(
				main(
					  h1("Index")
					, div(prologue)
					, h2("Games List")
					, ul(`class` := "gamesList")(
						  li(dataGame := EvosGame.values.to[Seq].head.name)(a(href := "shared/index.html")("Shared"))
						, frag(EvosGame.values.to[Seq].tail.map{game =>
							val name = game.toString
							li(dataGame := name)(
								a(href := (name + "/index.html"))(name)
							)
						}:_*)
					  )
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
				  title(s"Possible Evolutions - $config - ${checkMon.name}")
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
					, dl(
						dt("National Dex Number"),
							dd(checkMon.dexNo.toString),
						dt("Base Stat Total"),
							dd(checkMon.bst.toString),
						dt("Experience Group"),
							dd(checkMon.expGrowth),
						dt("Types"),
							dd(dataType := checkMon.types._1.toLowerCase, checkMon.types._1),
							if (checkMon.types._1 != checkMon.types._2) {
								dd(dataType := checkMon.types._2.toLowerCase, checkMon.types._2)
							} else {frag("")}
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
							, div("Natural: ", naturalEvo.name, " (BST = ", naturalBst, ")")
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
				(prevoDexno, prevoDexnoInfo) <- all.evolutions.to[Seq];
				(method, methodInfo) <- prevoDexnoInfo;
				(mGame, evoDexno) <- methodInfo if (game == mGame)
			) yield {
				((all.getPokemon(prevoDexno), method, all.getPokemon(evoDexno)))
			}
		}.sortBy{_._1.dexNo}
		
		html(lang := "en-US")(
			  head(
				  title(s"Possible Evolutions - $game")
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
				  		, table(
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
							all.firstStageMons.map{all.getPokemon _}
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
						, h2("Pokémon who eventually evolve into their vanilla final stage")
						, {
							val mons = for (
								startDexNo <- all.allDexNos;
								thisGameFinalEvo <- all.finalEvolutions(startDexNo);
								naturalFinalEvo <- all.finalEvolutions(startDexNo)(EvosGame.Natural)
											if (thisGameFinalEvo == naturalFinalEvo) 
							) yield { all.getPokemon(startDexNo) }
							
							pokemonListTable(mons, Seq.empty, all.possibleEvosCount, all.possiblePrevosCount)
						  }
						, if (game.monToMatch == MonToMatch.Neither) {frag(
							  h2("Pokémon with same-type evolutions")
							, table(
								thead(
									th("From DexNo"), th("From Name"),
									th("Shared Type"),
									th("To DexNo"), th("To Name")
								),
								tbody({
									evolutionList.flatMap{case (from, _, to) =>
										val (a1, a2) = from.types
										val (b1, b2) = to.types
										
										if (a1 == b1) {
											Seq((from, to, a1))
										} else if (a1 == b2) {
											Seq((from, to, a1))
										} else if (a2 == b1) {
											Seq((from, to, a2))
										} else if (a2 == b2) {
											Seq((from, to, a2))
										} else {
											Seq.empty
										}
									}.map{case (from, to, typ) =>
										tr(
											  td(dataSort := from.dexNo.toStringPadded)(from.dexNo.toString)
											, td(dataSort := from.name)(from.name)
											, td(dataType := typ.toLowerCase, dataSort := typ)(typ)
											, td(dataSort := to.dexNo.toStringPadded)(to.dexNo.toString)
											, td(dataSort := to.name)(to.name)
										)
									}:_*
								})
							  )
						  )} else {frag("")}
						, h2("4 stage evolution chains")
						, ul({
							all.threeEvoChains.map{case (p1,p2,p3,p4) => 
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
						, h2("Large evolution chain convergances")
						, ul(
							all.families.map{case (end, members) =>
								if (members.size >= 6) {frag(
									h4(a(href := s"${end}.html", all.getPokemon(end).name), " – ", members.size.toString),
									ul(
										members.to[Seq].map{member => li(member + " " + all.getPokemon(member).name)}:_*
									)
								)} else {frag("")}
							}.to[Seq]:_*
						  )
					)} else {frag("")}
				  )
			  )
		)
	}
	
	def sharedPage(all:ListOfPokemon):scalatags.Text.Frag = {
		html(lang := "en-US")(
			  head(
				  title(s"Possible Evolutions - Shared")
				, link(rel := "stylesheet", href := "../style/style.css")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/sectionCollapse.js")(" ")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/tableSort.js")(" ")
			  )
			, body(
				  header(
					a(href := "../index.html")("Back to Index")
				  )
				, main(
					  h1("Shared")
					, h2("Repeat Evolutions")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "5")
						, frag( EvosGame.values.map{x => colgroup(colgroupSpan := "1", dataGame := x.toString)}:_* )
						, thead(tr(
							  th("From Num")
							, th("From")
							, th("Method")
							, th("To")
							, th("To Num")
							, frag( EvosGame.values.map{x => th(x.shortName)}:_* )
						  ))
						, tbody(frag({
							for (
								(prevoNum, prevoData) <- all.evolutions.to[Seq].sortBy{_._1};
								(method, methodData) <- prevoData;
								(evoNum, games) <- methodData.groupBy{_._2}.mapValues{_.map{_._1}.to[Seq]} if games.size >= 2
							) yield {
								val evoName = all.getPokemon(evoNum).name
								val prevoName = all.getPokemon(prevoNum).name
								
								tr(
									  td(dataSort := prevoNum.toStringPadded, prevoNum.toString)
									, td(dataSort := prevoName, prevoName)
									, td(dataSort := method, method)
									, td(dataSort := evoNum.toStringPadded, evoNum.toString)
									, td(dataSort := evoName, evoName)
									, frag( EvosGame.values.map{x =>
										if (games contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
									  }:_*)
								)
							}
						  }:_*))
					  )
					, h2("Pokémon with multiple prevos multiple times")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "2")
						, frag( EvosGame.values.map{x => colgroup(colgroupSpan := "1", dataGame := x.toString)}:_* )
						, thead(tr(
							  th("DexNo")
							, th("Pokémon")
							, frag( EvosGame.values.map{x => th(x.shortName)}:_* )
						  ))
						, tbody(frag({
							val evolutions = for (
								(prevoNum, prevoData) <- all.evolutions.to[Seq].sortBy{_._1};
								(method, methodData) <- prevoData;
								(game, evoNo) <- methodData
							) yield { ((prevoNum, game, evoNo)) }
							val multiplePrevolutions = evolutions
								.groupBy{_._3}
								.mapValues{_.groupBy{_._2}}
								.mapValues{_.mapValues{_.map{_._1}}}
								.mapValues{_.filter{_._2.size >= 2}}
								.map{x => x}
								.filter{_._2.size >= 2}
								
							multiplePrevolutions.map{ab =>
								val (evoNum, games) = ab
								val evoName = all.getPokemon(evoNum).name
								
								tr(
									  td(dataSort := evoNum.toStringPadded, evoNum.toString)
									, td(dataSort := evoName, evoName)
									, frag( EvosGame.values.map{x =>
										if (games contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
									  }:_*)
								)
							}.to[Seq]
						  }:_*))
					  )
					, h2("Pokémon with no prevos multiple times")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "2")
						, frag( EvosGame.values.map{x => colgroup(colgroupSpan := "1", dataGame := x.toString)}:_* )
						, thead(tr(
							  th("DexNo")
							, th("Pokémon")
							, frag( EvosGame.values.map{x => th(x.shortName)}:_* )
						  ))
						, tbody(frag({
							all.allDexNos.flatMap{dexno =>
								val games = EvosGame.values.filter{game =>
									all.firstStageMons(game) contains dexno
								}
								
								if (games.size < 2) {
									Seq.empty
								} else {
									Seq((dexno, games))
								}
							}.map{case (dexNo, games) =>
								val name = all.getPokemon(dexNo).name
								tr(
									  td(dataSort := dexNo.toStringPadded, dexNo.toString)
									, td(dataSort := name, name)
									, frag( EvosGame.values.map{x =>
										if (games contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
									  }:_*)
								)
							}.toSeq:_*
						  }))
					  )
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
		, (if (! config.showSeedData) {frag("")} else {frag(
			  h4("Observed")
			, {
				val observedThisGame = observed.filter{_._1 == config}.map{_._2}.map(resolveDexNo).to[Seq]
				if (observedThisGame.size == 0) {
					p("None")
				} else {
					pokemonListTable(observedThisGame, observed, possibleEvosCount, possiblePrevosCount)
				}
			  }
		  )})
		, h4("Candidates")
		, {
			if (possible.size == 0) {
				p("None")
			} else {
				pokemonListTable(possible, observed, possibleEvosCount, possiblePrevosCount)
			}
		  }
	)
	
	
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
			  td(dataSort := x.dexNo.toStringPadded)(x.dexNo.toString)
			, td(dataSort := x.name)(
				a(href := s"${x.dexNo}.html")(x.name)
			  )
			, td(dataSort := x.types._1, dataType := x.types._1.toLowerCase)(x.types._1)
			, td(dataSort := x.types._2, dataType := x.types._2.toLowerCase)(x.types._2) 
			, td(dataSort := padStrWithZeros(x.bst))(x.bst.toString)
			, td(dataSort := x.expGrowth)(x.expGrowth)
			, td(dataSort := padStrWithZeros(possibleEvosCount(x.dexNo)))(possibleEvosCount(x.dexNo).toString)
			, td(dataSort := padStrWithZeros(possiblePrevosCount(x.dexNo)))(possiblePrevosCount(x.dexNo).toString)
		)
	}
	
	private[this] def padStrWithZeros(x:Int):String = ("00000" + x).takeRight(5)
	
}
