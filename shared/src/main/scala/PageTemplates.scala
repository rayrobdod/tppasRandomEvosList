package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.Seq
import scalatags.generic.Bundle
import scalatags.generic.Frag
import com.rayrobdod.possibleEvolutions.DexNo.mapCanBuildFrom

object PageTemplatesText extends PageTemplates(
	  scalatags.Text
	, scalatags.Text.implicits.raw("<!DOCTYPE html>\n")
)

class PageTemplates[Builder, Output <: FragT, FragT](
	  bundle:Bundle[Builder, Output, FragT]
	, htmlDoctype:Frag[Builder, FragT]
) {
	import bundle.tags.{attr => _, frag => _, _}
	import bundle.attrs.{tag => _, modifier => _, title => _, _}
	import bundle.implicits._

	private[this] val main = tag("main")
	private[this] val defer = attr("defer")
	private[this] val colgroupSpan = attr("span")
	private[this] val dataGame = data("game")
	private[this] val dataSort = data("sort")
	private[this] val dataType = data("type")
	private[this] val title = tag("title")
	
	
	def index(prologue:scalatags.generic.Frag[Builder,FragT], gameNames:Seq[String]):scalatags.generic.Frag[Builder,FragT] = {
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
						  li(dataGame := "natural")(a(href := "shared/index.html")("Shared"))
						, frag(gameNames.map{name =>
							li(dataGame := name)(
								a(href := (name + "/index.html"))(name)
							)
						  }:_*)
						, li(dataGame := "theoretical")(a(href := "theoretical/index.html")("Theoretical"))
					  )
				)
			  )
		))
	}
	
	def perMonPage(
			  monNo:DexNo
			, predictions:Predictor
			, game:EvosGame.Value
			, seedDatas:Seq[SeedData]
	):scalatags.generic.Frag[Builder,FragT] = {
		implicit val config = game
		
		val checkMon = AllPokemon.get(monNo).get
		val evos:Map[String, Seq[Pokemon]] = predictions.possibleEvolutions(monNo)
		val realEvos:Map[EvosGame.Value, Map[String,DexNo]] = seedDatas.map{s => ((s.game, s.evolutions.getOrElse(monNo, Map.empty)))}.toMap
		val prevos = predictions.possiblePrevolutions(monNo)
		val realPrevos = seedDatas.map{s => ((s.game, s.prevos.getOrElse(monNo, Set.empty)))}.flatMap{case (a,bs) => bs.map{b => ((a,b))}}
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
							val bstRange = config.bstMatchFunction match {
								case BstMatchFunction.Any => ""
								case BstMatchFunction.Pk3ds => s"stat_total=${naturalBst * 5 / 6}-${naturalBst * 6 / 5}"
								case BstMatchFunction.UniversalRandomizer => s"stat_total=${naturalBst * 9 / 10}-${naturalBst * 11 / 10}"
								case BstMatchFunction.Custom(min, max) => s"stat_total=${(naturalBst * min).intValue}-${(naturalBst * max).intValue}"
							}
							val generation = s"&id=<=${config.maxKnownDexno}"
							
							(
								"http://veekun.com/dex/pokemon/search?" +
								bstRange +
								generation +
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
	):scalatags.generic.Frag[Builder,FragT] = {
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
						  dt("Pokémon")
						, dd(game.maxKnownDexno match {
							case DexNo(151) => "Gen 1"
							case DexNo(251) => "Gen 2"
							case DexNo(386) => "Gen 3"
							case DexNo(493) => "Gen 4"
							case DexNo(649) => "Gen 5"
							case DexNo(721) => "Gen 6"
							case DexNo(802) => "Gen 7"
							case DexNo(x) => s"Up to $x inclusive"
						  })
						, dt("Base Stat Totals")
						, dd(game.bstType match {
							case MonBstType.Gen1 => "Gen 1"
							case MonBstType.Gen2 => "Gen 2 - 5"
							case MonBstType.Gen6 => "Gen 6"
							case MonBstType.Gen7 => "Gen 7"
						  })
						, dt("Pokémon Types")
						, dd(game.typeType match {
							case MonTypeType.Natural => "Natural (Gen 6+)"
							case MonTypeType.NoFairy => "Natural (Gen 2-5)"
							case MonTypeType.RandPlat => "Randomized Platinum's Types"
						  })
						, dt("Type Match")
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
						, dd(game.bstMatchFunction.description)
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
	
	def sharedPage(seedDatas:Seq[SeedData]):scalatags.generic.Frag[Builder,FragT] = {
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
						, frag( seedDatas.map{x => colgroup(colgroupSpan := "1", dataGame := x.game.toString)}:_* )
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
						, frag( seedDatas.map{x => colgroup(colgroupSpan := "1", dataGame := x.game.toString)}:_* )
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
						, frag( seedDatas.map{x => colgroup(colgroupSpan := "1", dataGame := x.game.toString)}:_* )
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
	
	def sharedEeveePage(seedDatas:Seq[SeedData]):scalatags.generic.Frag[Builder,FragT] = {
		val eeveeDexNo = new DexNo(133)
		val nameHeaders = seedDatas.map{_.game.shortName}.map{x => th(x)}
		
		val methodIcons:Map[String, String] = Map(
			"Used Item [Water Stone]" -> "🌊", // ☂
			"Used Item [Thunder Stone]" -> "💡", // ☈
			"Used Item [Fire Stone]" -> "🔥",
			"Level Up at Morning" -> "☀",
			"Level Up at Night" -> "☽",
			"Level Up at Forest" -> "🍂",
			"Level Up at Cold" -> "❄", //⛇. ☃
			"Level Up with 50 Affection + MoveType [Fairy]" -> "❤" //♥
		)
		val methodSorting:Map[String, String] = Map(
			"Used Item [Water Stone]" -> "1",
			"Used Item [Thunder Stone]" -> "2",
			"Used Item [Fire Stone]" -> "3",
			"Level Up at Morning" -> "4",
			"Level Up at Night" -> "5",
			"Level Up at Forest" -> "6",
			"Level Up at Cold" -> "7",
			"Level Up with 50 Affection + MoveType [Fairy]" -> "8"
		)
		
		frag(htmlDoctype, html(lang := "en-US")(
			  head(
				  title(s"Possible Evolutions - Shared - Eevee")
				, meta(charset := "utf-8")
				, link(rel := "stylesheet", href := "../style/style.css")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/sectionCollapse.js")(" ")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/tableSort.js")(" ")
			  )
			, body(
				  header(
					a(href := "../index.html")("Back to Index")
					, " > "
					, a(href := "index.html")("Game")
				  )
				, main(
					  h1("Eevee")
					, h2("Evolutions")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "2")
						, frag( seedDatas.map{x => colgroup(colgroupSpan := "1", dataGame := x.game.toString)}:_* )
						, thead(tr(
							th("To Num"),
							th("To"),
							frag( nameHeaders:_* )
						  ))
						, tbody(frag({
							val eeveeEvos:Map[DexNo, Map[SeedData, Seq[String]]] = {
								val a:Seq[(DexNo, SeedData, String)] = for (
									seedData <- seedDatas;
									(method, toNo) <- seedData.evolutions(eeveeDexNo)
								) yield {(toNo, seedData, method)}
								a.groupBy{_._1}.mapValues{_.groupBy{_._2}.mapValues{_.map{_._3}}.map{x => x}}.map{x => x}
							}
							def nameOf(dexNo:DexNo) = AllPokemon.get(dexNo).map{_.name}.getOrElse{"???"}
							
							eeveeEvos.to[Seq].map{case (toNo, gameMethods) => tr(
								td(dataSort := toNo.toString, toNo.toString),
								td(dataSort := nameOf(toNo), nameOf(toNo)),
								frag(seedDatas.map{game =>
									val methods = gameMethods.getOrElse(game, Seq.empty)
									val sortStr = {
										val a = methods.map{method => methodSorting.getOrElse(method, "-1")}.mkString
										if (a == "") {"999"} else {a}
									}
									val iconStr = methods.map{method => methodIcons.getOrElse(method, "?")}.mkString
									td(dataSort := sortStr, iconStr)
								}:_*)
							)}
						  }:_*))
					  )
				  )
			  )
		))
	}
	
	def theoreticalPage:scalatags.generic.Frag[Builder,FragT] = {
		
		def checkbox(labelStr:String, idStr:String) = div(
			  input(`type` := "checkbox", name := idStr, id := idStr)
			, label(labelStr, `for` := idStr)
		)
		def options(labelStr:String, idStr:String, optionStrs:Seq[(String, String)]) = fieldset(
			  legend(labelStr)
			, ul(
				optionStrs.zipWithIndex.map{case ((labelStr, valueStr), idx) => li(
					  input(`type` := "radio", id := s"${idStr}_${valueStr}", name := idStr, value := valueStr, if (idx == 0) {checked := "checked"} else {""})
					, label(labelStr, `for` := s"${idStr}_${valueStr}")
				)}:_*
			  )
		)
		
		frag(htmlDoctype, html(lang := "en-US")(
			  head(
				  title(s"Possible Evolutions - Theoretical")
				, link(rel := "stylesheet", href := "../style/style.css")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/sectionCollapse.js")(" ")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/tableSort.js")(" ")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/theoreticalPage.js")(" ")
				, script(defer := "defer", `type` := "text/javascript")(raw("""
				  |document.addEventListener("DOMContentLoaded", function() {
				  |	function forEach(coll, fun) {
				  |		for (i = 0; i < coll.length; i++) {
				  |			fun(coll[i]);
				  |		}
				  |	}
				  |
				  |	function syncBstEnabled() {
				  |		var checked = document.getElementById("bstdifference_custom").checked;
				  |		document.getElementById("bstdifference_min").disabled = !checked;
				  |		document.getElementById("bstdifference_max").disabled = !checked;
				  |	}
				  |	
				  |	syncBstEnabled();
				  |	forEach(document.querySelectorAll("input[name=\"bstdifference\"]"), function(input) {
				  |		input.addEventListener("change", syncBstEnabled);
				  |	});
				  |})
				  |""".stripMargin.replace("\n", "").replace("\r", "").replace("\t", "")))
			  )
			, body(
				  header(
					a(href := "../index.html")("Back to Index")
				  )
				, main(
					  p(
					  	"Note that these calculations are done in the browser,so if ",
					  	"scripting is disabled this won't be able to work."
					  )
					, h1("Settings")
					, form(`id` := "theoretical-game-properties",
						  h2("Baseline Information")
						, options("Generation", "generation", Seq(
							  "Gen1" -> "151"
							, "Gen2" -> "251"
							, "Gen3" -> "386"
							, "Gen4" -> "493"
							, "Gen5" -> "649"
							, "Gen6" -> "721"
							, "Gen7" -> "802"
						  ))
						, options("Types", "monTypeType", Seq(
							  "Normal" -> MonTypeType.Natural.id.toString
							, "Normal Sans Fairy" -> MonTypeType.NoFairy.id.toString
							, "Random Platinum" -> MonTypeType.RandPlat.id.toString
						  ))
						, options("BST Values", "monBstType", Seq(
							  "Gen1" -> MonBstType.Gen1.id.toString
							, "Gen2-5" -> MonBstType.Gen2.id.toString
							, "Gen6" -> MonBstType.Gen6.id.toString
							, "Gen7" -> MonBstType.Gen7.id.toString
						  ))
						, h2("Evolution Restrictions")
						, checkbox("Experience Group Match", "expGroup")
						, checkbox("Natural Evolution Allowed", "naturalEvolution")
						, options("New evolution must match", "monTypeToMatch", Seq(
							  "Neither" -> MonTypeToMatch.Neither.id.toString
							, "Base Form" -> MonTypeToMatch.BaseForm.id.toString
							, "Evolved Form" -> MonTypeToMatch.EvolvedForm.id.toString
						  ))
						, fieldset(
							  legend("Allowed difference in BST")
							, ul(
							  	  li(
									  input(`type` := "radio", id := "bstdifference_any", name := "bstdifference", value := "any", checked := "checked")
									, label("Any", `for` := "bstdifference_any")
								  )
							  	, li(
									  input(`type` := "radio", id := "bstdifference_pk", name := "bstdifference", value := "pk")
									, label("5/6 to 6/5 (Like pk3DS)", `for` := "bstdifference_pk")
								  )
							  	, li(
									  input(`type` := "radio", id := "bstdifference_ur", name := "bstdifference", value := "ur")
									, label("9/10 to 11/10 (like Universal Randomizer)", `for` := "bstdifference_ur")
								  )
								, fieldset(
									legend(
										  input(`type` := "radio", id := "bstdifference_custom", name := "bstdifference", value := "custom")
										, label("Custom", `for` := "bstdifference_custom")
									)
									, div(
										  label("Minimum: ", `for` := "bstdifference_min")
										, input(`type` := "number", name := "bstdifference_min", id := "bstdifference_min", step := 0.1, max := 1, min := 0, value := "0.9")
									  )
									, div(
										  label("Maximum: ", `for` := "bstdifference_max")
										, input(`type` := "number", name := "bstdifference_max", id := "bstdifference_max", step := 0.1, max := 10, min := 1, value := "1.1")
									  )
								)
							)
						  )
						, h2("Generate")
						, button("Generate", `type` := "button", id := "generate")
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
	):scalatags.generic.Frag[Builder,FragT] = frag(
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
	
	
	def pokemonListTable(
			  x:Iterable[Pokemon]
			, realEvos:Iterable[(EvosGame.Value, DexNo)]
			, possibleEvosCount:DexNo => Int
			, possiblePrevosCount:DexNo => Int
			)(implicit config:EvosGame.Value
	):scalatags.generic.Frag[Builder,FragT] = {
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
	):scalatags.generic.Frag[Builder,FragT] = {
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
