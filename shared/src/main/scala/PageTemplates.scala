package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.Seq
import scalatags.Text.tags.{attr => _, frag => _, _}
import scalatags.Text.attrs.{tag => _, modifier => _, title => _, _}
import scalatags.Text.implicits._

object PageTemplates {
	private[this] val main = tag("main")
	private[this] val defer = attr("defer")
	private[this] val colgroupSpan = attr("span")
	private[this] val dataGame = data("game")
	private[this] val dataSort = data("sort")
	private[this] val dataType = data("type")
	private[this] val title = tag("title")
	
	
	def index(prologue:scalatags.Text.Frag):scalatags.Text.Frag = {
		frag(htmlDoctype, html(lang := "en-US")(
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
		))
	}
	
	def perMonPage(
			  monNo:DexNo
			, predictions:Predictor
			, game:EvosGame.Value
	):scalatags.Text.Frag = {
		implicit val config = game
		
		val checkMon = AllPokemon.get(monNo).get
		val evos:Map[String, Seq[Pokemon]] = predictions.possibleEvolutions(monNo)
		val realEvos:Map[EvosGame.Value, Map[String,DexNo]] = EvosGame.values.flatMap{g => g.seedData.map{s => ((g, s.evolutions.getOrElse(monNo, Map.empty)))}}.toMap
		val prevos = predictions.possiblePrevolutions(monNo)
		val realPrevos = EvosGame.values.flatMap{g => g.seedData.map{s => ((g, s.prevos.getOrElse(monNo, Set.empty)))}}.flatMap{case (a,bs) => bs.map{b => ((a,b))}}
		val prevos2 = prevos.flatMap{mon => predictions.possiblePrevolutions(mon.dexNo)}.distinct
		val realPrevos2 = {
			for (
				seedData <- game.seedData.to[Seq];
				(game, prevo) <- realPrevos;
				prevo2 <- seedData.prevos.getOrElse(prevo, Set.empty)
			) yield {
				(game, prevo2)
			}
		}.distinct
		
		frag(htmlDoctype, html(lang := "en-US")(
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
						val naturalEvo = predictions.getPokemon(realEvos(EvosGame.Natural)(method))
						val naturalBst = naturalEvo.bst
						val realEvosMethod = realEvos.flatMap{case (a,bs) => bs.get(method).map{b => ((a, b))}}
						
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
								case MonTypeToMatch.Neither => ""
								case MonTypeToMatch.BaseForm => {
									val (type1, type2) = checkMon.types
									s"&type=${type1.toLowerCase}&type=${type2.toLowerCase}"
								}
								case MonTypeToMatch.EvolvedForm => {
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
							, monPredictionSection(possibleEvos, realEvosMethod, predictions.possibleEvosCount, predictions.possiblePrevosCount, predictions.getPokemon)
						)
					  }.to[Seq]:_*)
					, h2("Possible Prevos")
					, monPredictionSection(prevos, realPrevos, predictions.possibleEvosCount, predictions.possiblePrevosCount, predictions.getPokemon)
					, h2("Possible Prevos^2")
					, monPredictionSection(prevos2, realPrevos2, predictions.possibleEvosCount, predictions.possiblePrevosCount, predictions.getPokemon)
				)
			)
		))
	}
	
	def perGamePage(
			  predictions:Predictor
			, game:EvosGame.Value
	):scalatags.Text.Frag = {
		implicit val config:EvosGame.Value = game
		
		frag(htmlDoctype, html(lang := "en-US")(
			  head(
				  title(s"Possible Evolutions - ${game.name}")
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
				  	, dl(
				  		  dt("Type Match")
				  		, dd(game.monToMatch match {
				  			case MonTypeToMatch.BaseForm => "Base Form"
				  			case MonTypeToMatch.EvolvedForm => "Natural Evolution"
				  			case MonTypeToMatch.Neither => "Off"
				  		  })
				  		, dt("Experience Group Match")
				  		, dd(if (game.expGroupMustMatch) {"Yes"} else {"No"})
				  		, dt("Natural Evolution Allowed")
				  		, dd(if (game.naturalEvoAllowed) {"Yes"} else {"No"})
				  		, dt("Base Stat Total Range")
				  		, dd(game.bstMatchString)
				  	  )
				  	, h2("Pokémon List")
				  	, pokemonListTable(
				  		  predictions.extantPokemon
				  		, Map.empty
				  		, predictions.possibleEvosCount
				  		, predictions.possiblePrevosCount
				  	)
				  	)(game.seedData.map{seedData => frag(
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
								(for (
									(fromNo, methodto) <- seedData.evolutions.to[Seq];
									(method, toNo) <- methodto.to[Seq]
								) yield {
									val from = predictions.getPokemon(fromNo)
									val to = predictions.getPokemon(toNo)
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
								}):_*
							  )
				  		  )
						, h2("Pokémon that nothing evolves into")
						, pokemonListTable(
							  seedData.firstStageMons.map{predictions.getPokemon _}
							, Seq.empty, predictions.possibleEvosCount, predictions.possiblePrevosCount
						  )
						, h2("Pokémon that multiple things evolve into")
						, pokemonListTable(
							  seedData.multiplePrevos.map{predictions.getPokemon _}
							, Seq.empty, predictions.possibleEvosCount, predictions.possiblePrevosCount
						  )
						, h2("Pokémon whose evo isn't predicted")
						, pokemonListTable(
							(for (
								(prevoNo, prevonodata) <- seedData.evolutions.to[Seq];
								(method, realEvoNo) <- prevonodata.to[Seq]
							) yield {
								val isPredicted = predictions.possibleEvolutions(prevoNo)(method).map{_.dexNo} contains realEvoNo
								
								if (isPredicted) {
									Seq.empty
								} else {
									Seq(prevoNo)
								}
							}).flatten.to[Seq].distinct.map{predictions.getPokemon _}
							, Seq.empty, predictions.possibleEvosCount, predictions.possiblePrevosCount
						  )
						, h2("Pokémon who eventually evolve into their vanilla final stage")
						, {
							val mons = for (
								startDexNo <- predictions.extantPokemon.map{_.dexNo};
								thisGameFinalEvo <- seedData.finalEvolutions(startDexNo);
								naturalFinalEvo <- evolutionData.Natural.finalEvolutions(startDexNo)
											if (thisGameFinalEvo == naturalFinalEvo) 
							) yield { predictions.getPokemon(startDexNo) }
							
							pokemonListTable(mons, Seq.empty, predictions.possibleEvosCount, predictions.possiblePrevosCount)
						  }
						, if (game.monToMatch == MonTypeToMatch.Neither) {frag(
							  h2("Pokémon with same-type evolutions")
							, table(
								thead(
									th("From DexNo"), th("From Name"),
									th("Shared Type"),
									th("To DexNo"), th("To Name")
								),
								tbody({
									def haveSharedType(
										a:Pokemon, b:Pokemon
									):Seq[String] = {
										val (a1, a2) = a.types
										val (b1, b2) = b.types
										
										if (a1 == b1) {
											Seq(a1)
										} else if (a1 == b2) {
											Seq(a1)
										} else if (a2 == b1) {
											Seq(a2)
										} else if (a2 == b2) {
											Seq(a2)
										} else {
											Seq.empty
										}
									}
									
									val elems = (for (
										(fromNo, toNos) <- seedData.evolutions.to[Seq];
										(method, toNo) <- toNos.to[Seq];
										fromMon <- Seq(predictions.getPokemon(fromNo));
										toMon <- Seq(predictions.getPokemon(toNo));
										typ <- haveSharedType(fromMon, toMon)
									) yield {
										tr(
											  td(dataSort := fromMon.dexNo.toStringPadded)(fromMon.dexNo.toString)
											, td(dataSort := fromMon.name)(fromMon.name)
											, td(dataType := typ.toLowerCase, dataSort := typ)(typ)
											, td(dataSort := toMon.dexNo.toStringPadded)(toMon.dexNo.toString)
											, td(dataSort := toMon.name)(toMon.name)
										)
									})
									
									frag(elems:_*)
								})
							  )
						  )} else {frag("")}
						, h2("4 stage evolution chains")
						, ul({
							seedData.threeEvoChains.map{case (p1,p2,p3,p4) => 
								li(
									  a(href := s"${p1}.html")(s"${p1} ${predictions.getPokemon(p1).name}")
									, " → "
									, a(href := s"${p2}.html")(s"${p2} ${predictions.getPokemon(p2).name}")
									, " → "
									, a(href := s"${p3}.html")(s"${p3} ${predictions.getPokemon(p3).name}")
									, " → "
									, a(href := s"${p4}.html")(s"${p4} ${predictions.getPokemon(p4).name}")
								)
							}
						  }:_*)
						, h2("Large evolution chain convergances")
						, ul(
							seedData.families.map{case (end, members) =>
								if (members.size >= 6) {frag(
									h4(a(href := s"${end}.html", predictions.getPokemon(end).name), " – ", members.size.toString),
									ul(
										members.to[Seq].map{member => li(member + " " + predictions.getPokemon(member).name)}:_*
									)
								)} else {frag("")}
							}.to[Seq]:_*
						  )
					)}.getOrElse(frag(""))
				  )
			  )
		))
	}
	
	def sharedPage:scalatags.Text.Frag = {
		val seedDatas:Seq[SeedData] = EvosGame.values.flatMap{_.seedData}
		val nameHeaders = seedDatas.map{_.game.shortName}.map{x => th(x)}
		
		frag(htmlDoctype, html(lang := "en-US")(
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
							, frag( nameHeaders:_* )
						  ))
						, tbody(frag({
							for (
								prevo <- AllPokemon.apply;
								(method, _) <- evolutionData.Natural.evolutions(prevo.dexNo)
							) yield {
								val evos:Seq[(SeedData, DexNo)] = {
									for (
										seedData <- seedDatas;
										prevoData <- seedData.evolutions.get(prevo.dexNo);
										methodData <- prevoData.get(method)
									) yield { (seedData, methodData) }
								}
								
								val multiGameEvos:Map[DexNo, Seq[SeedData]] = {
									evos.groupBy{_._2}.mapValues{_.map{_._1}}.filter{_._2.size >= 2}
								}
								
								multiGameEvos.to[Seq].map{case (evoNum, games) =>
									val evoName = AllPokemon.get(evoNum).get.name
									val prevoNum = prevo.dexNo
									val prevoName = prevo.name
									
									tr(
										  td(dataSort := prevoNum.toStringPadded, prevoNum.toString)
										, td(dataSort := prevoName, prevoName)
										, td(dataSort := method, method)
										, td(dataSort := evoNum.toStringPadded, evoNum.toString)
										, td(dataSort := evoName, evoName)
										, frag( seedDatas.map{x =>
											if (games contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
										  }:_*)
									)
								}
							}
						  }.flatten:_*))
					  )
					, h2("Pokémon with multiple prevos multiple times")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "2")
						, frag( EvosGame.values.map{x => colgroup(colgroupSpan := "1", dataGame := x.toString)}:_* )
						, thead(tr(
							  th("DexNo")
							, th("Pokémon")
							, frag( nameHeaders:_* )
						  ))
						, tbody(frag({
							AllPokemon.apply.map{mon =>
								val games = seedDatas.filter{_.multiplePrevos contains mon.dexNo}
								if (games.size >= 2) {
									val evoNum = mon.dexNo
									val evoName = mon.name
									
									tr(
										  td(dataSort := evoNum.toStringPadded, evoNum.toString)
										, td(dataSort := evoName, evoName)
										, frag( seedDatas.map{x =>
											if (games contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
										  }:_*)
									)
								} else {
									frag("")
								}
							}
						  }:_*))
					  )
					, h2("Pokémon with no prevos multiple times")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "2")
						, frag( EvosGame.values.map{x => colgroup(colgroupSpan := "1", dataGame := x.toString)}:_* )
						, thead(tr(
							  th("DexNo")
							, th("Pokémon")
							, frag( nameHeaders:_* )
						  ))
						, tbody(frag({
							AllPokemon.apply.map{mon =>
								val games = seedDatas.filter{_.firstStageMons contains mon.dexNo}
								if (games.size >= 2) {
									val evoNum = mon.dexNo
									val evoName = mon.name
									
									tr(
										  td(dataSort := evoNum.toStringPadded, evoNum.toString)
										, td(dataSort := evoName, evoName)
										, frag( seedDatas.map{x =>
											if (games contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
										  }:_*)
									)
								} else {
									frag("")
								}
							}
						  }:_*))
					  )
				  )
			  )
		))
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
		, config.seedData.map{seedData => frag(
			  h4("Observed")
			, {
				val observedThisGame = observed.filter{_._1 == config}.map{_._2}.map(resolveDexNo).to[Seq]
				if (observedThisGame.size == 0) {
					p("None")
				} else {
					pokemonListTable(observedThisGame, observed, possibleEvosCount, possiblePrevosCount)
				}
			  }
		  )}.getOrElse(frag(""))
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
	
	private[this] def pokemonTableRow(
			  realEvos:Iterable[(EvosGame.Value, DexNo)]
			, possibleEvosCount:DexNo => Int
			, possiblePrevosCount:DexNo => Int
			)(x:Pokemon
			)(implicit config:EvosGame.Value
	):scalatags.Text.TypedTag[String] = {
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
