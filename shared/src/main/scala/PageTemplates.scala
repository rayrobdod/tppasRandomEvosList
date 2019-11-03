package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.Seq
import scala.collection.immutable.SortedSet
import scalatags.generic.Bundle
import scalatags.generic.Frag
import com.rayrobdod.possibleEvolutions.seedData.{Natural => NaturalSeedData}

object PageTemplatesText extends PageTemplates(
	  scalatags.Text
	, scalatags.Text.implicits.raw("<!DOCTYPE html>" + System.lineSeparator)
)

/**
 * The templates for the HTML output files.
 */
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
	
	private[this] val hrefDexNoLinkModifier:Function[DexNo, scalatags.generic.Modifier[Builder]] = {
		(dexNo) => href := s"${dexNo}.html"
	}
	
	private[this] val runToColgroup:Function[Run, scalatags.generic.Frag[Builder,FragT]] = {
		(run) => colgroup(colgroupSpan := "1", dataGame := run.name)
	}

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
			, game:Runs.Value
	):scalatags.generic.Frag[Builder,FragT] = {
		implicit val settings = RandomizerSettings.runToValue(game)
		val seedDatas = SeedData.runToValue.get(game)
		val checkMon = AllPokemon.get(monNo).get
		
		frag(htmlDoctype, html(lang := "en-US")(
			  head(
				  title(s"Possible Evolutions - ${game.name} - ${checkMon.name}")
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
					h1(checkMon.name),
					perMonMain(monNo, predictions, settings, seedDatas, hrefDexNoLinkModifier)
				  )
			)
		))
	}
	
	def perMonMain(
			  monNo:DexNo
			, predictions:Predictor
			, settings:RandomizerSettings
			, seedDatas:Option[SeedData]
			, dexNoLinkModifier:DexNo => scalatags.generic.Modifier[Builder]
	):scalatags.generic.Frag[Builder,FragT] = {
		implicit val typeType:MonTypeType.Value = settings.typeType
		implicit val bstType:MonBstType.Value = settings.bstType
		val checkMon = AllPokemon.get(monNo).get
		val evos:Map[String, Seq[Pokemon]] = predictions.possibleEvolutions(monNo)
		val realEvos:Map[Runs.Value, Map[String,DexNo]] = SeedData.runToValue.mapValues({seedData => seedData.evolutions.getOrElse(monNo, Map.empty)}).map{x => x}
		val naturalEvos:Map[String, DexNo] = seedData.Natural.evolutions.getOrElse(monNo, Map.empty)
		val prevos = predictions.possiblePrevolutions(monNo)
		val realPrevos = SeedData.runToValue.mapValues({seedData => seedData.prevos.getOrElse(monNo, Set.empty)}).to[Seq].flatMap{case (a,bs) => bs.map{b => ((a,b))}}
		val prevos2 = prevos.flatMap{mon => predictions.possiblePrevolutions(mon.dexNo)}.distinct
		val realPrevos2 = {
			for (
				seedData <- seedDatas.to[Seq];
				(game, prevo) <- realPrevos.to[Seq];
				prevo2 <- seedData.prevos.getOrElse(prevo, Set.empty).to[Seq]
			) yield {
				(game, prevo2)
			}
		}.distinct
		
		frag(
			  dl(
				dt("National Dex Number"),
					dd(checkMon.dexNo.toString),
				dt("Base Stat Total"),
					dd(checkMon.bst.toString),
				dt("Experience Group"),
					dd(checkMon.expGrowth.toString),
				dt("Types"),
					dd(dataType := checkMon.types._1.toString.toLowerCase, checkMon.types._1.toString),
					if (checkMon.types._1 != checkMon.types._2) {
						dd(dataType := checkMon.types._2.toString.toLowerCase, checkMon.types._2.toString)
					} else {frag("")}
			  )
			, h2("Possible Evos")
			, div(evos.toSeq.sortBy{x => naturalEvos(x._1)}.flatMap{case (method:String, possibleEvos:Seq[Pokemon]) =>
				val naturalEvo = predictions.getPokemon(naturalEvos(method))
				val naturalBst = naturalEvo.bst
				val realEvosMethod = realEvos.flatMap{case (a,bs) => bs.get(method).map{b => ((a, b))}}
				
				val veekunSearchLink = {
					import com.rayrobdod.possibleEvolutions.ExperienceGrowth._
					val growthRate = checkMon.expGrowth match {
						case Slow => "1250000"
						case MedFast => "1000000"
						case Fast => "800000"
						case MedSlow => "1059860"
						case Erratic => "600000"
						case Fluctuating => "1640000"
					}
					val types = settings.typeMatch match {
						case MonTypeToMatch.Neither => ""
						case MonTypeToMatch.BaseForm => {
							val (type1, type2) = checkMon.types
							s"&type=${type1.toString.toLowerCase}&type=${type2.toString.toLowerCase}"
						}
						case MonTypeToMatch.EvolvedForm => {
							val (type1, type2) = naturalEvo.types
							s"&type=${type1.toString.toLowerCase}&type=${type2.toString.toLowerCase}"
						}
					}
					val bstRange = settings.bstMatchFunction match {
						case BstMatchFunction.Any => ""
						case BstMatchFunction.Pk3ds => s"stat_total=${naturalBst * 5 / 6}-${naturalBst * 6 / 5}"
						case BstMatchFunction.Pk3ds_2 => s"stat_total=${naturalBst * 10 / 11}-${naturalBst * 11 / 10}"
						case BstMatchFunction.UniversalRandomizer => s"stat_total=${naturalBst * 9 / 10}-${naturalBst * 11 / 10}"
						case BstMatchFunction.GoDTool => s"stat_total=${checkMon.bst}-${checkMon.bst * 10}"
						case BstMatchFunction.Custom(min, max) => s"stat_total=${(naturalBst * min).intValue}-${(naturalBst * max).intValue}"
					}
					val generation = settings.knownDexnos match {
						case DexNo.NationalDexNoRange(1, high) => s"&id=<=${high}"
						case _ => "&id=???"
					}
					
					(
						"http://veekun.com/dex/pokemon/search?" +
						bstRange +
						generation +
						(if (settings.expGroupMustMatch) {s"&growth_rate=$growthRate"} else {""}) ++
						types
					)
				}
				
				Seq(
					  h3(method)
					, div("Natural: ", naturalEvo.name, " (BST = ", naturalBst, ")")
					, div(a(href := veekunSearchLink)("Veekun search with same parameters"))
					, monPredictionSection(possibleEvos, settings, realEvosMethod, predictions.possibleEvosCount, predictions.possiblePrevosCount, predictions.getPokemon, dexNoLinkModifier, seedDatas)
				)
			  }.to[Seq]:_*)
			, h2("Possible Prevos")
			, monPredictionSection(prevos, settings, realPrevos, predictions.possibleEvosCount, predictions.possiblePrevosCount, predictions.getPokemon, dexNoLinkModifier, seedDatas)
			, h2("Possible Prevos^2")
			, monPredictionSection(prevos2, settings, realPrevos2, predictions.possibleEvosCount, predictions.possiblePrevosCount, predictions.getPokemon, dexNoLinkModifier, seedDatas)
		)
	}
	
	def perGamePage(
			  predictions:Predictor
			, game:Runs.Value
	):scalatags.generic.Frag[Builder,FragT] = {
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
					h1(game.name),
					perGameMain(
						predictions,
						RandomizerSettings.runToValue(game),
						SeedData.runToValue.get(game),
						hrefDexNoLinkModifier,
					)
				  )
			  )
		))
	}
	
	private val Gen7Dex = DexNo.NationalDexNoRange(1, 802) ++ DexNo.alolanDexNos
	private val Gen7PlusDex = DexNo.NationalDexNoRange(1, 807) ++ DexNo.alolanDexNos :+ DexNo.duskRockruff
	
	def perGameMain(
		predictions:Predictor,
		settings:RandomizerSettings,
		seedDatas:Option[SeedData],
		dexNoLinkModifier:DexNo => scalatags.generic.Modifier[Builder],
	):scalatags.generic.Frag[Builder,FragT] = {
		implicit val typeType:MonTypeType.Value = settings.typeType
		
		frag(
			frag(
				  dl(
					  dt("Pokémon")
					, dd(settings.knownDexnos match {
						case DexNo.NationalDexNoRange(1, 151) => "Gen 1"
						case DexNo.NationalDexNoRange(1, 251) => "Gen 2"
						case DexNo.NationalDexNoRange(1, 386) => "Gen 3"
						case DexNo.NationalDexNoRange(1, 493) => "Gen 4"
						case DexNo.NationalDexNoRange(1, 649) => "Gen 5"
						case DexNo.NationalDexNoRange(1, 721) => "Gen 6"
						case Gen7Dex => "Gen 7"
						case Gen7PlusDex => "Gen 7+"
						case DexNo.NationalDexNoRange(1, x) => s"Up to $x inclusive"
						case _ => "Something complicated"
					  })
					, dt("Base Stat Totals")
					, dd(settings.bstType match {
						case MonBstType.Gen1 => "Gen 1"
						case MonBstType.Gen2 => "Gen 2 - 5"
						case MonBstType.Gen6 => "Gen 6"
						case MonBstType.Gen7 => "Gen 7"
					  })
					, dt("Pokémon Types")
					, dd(settings.typeType match {
						case MonTypeType.Natural => "Natural (Gen 6+)"
						case MonTypeType.NoFairy => "Natural (Gen 2-5)"
						case MonTypeType.RandPlat => "Randomized Platinum's Types"
					  })
					, dt("Type Match")
					, dd(settings.typeMatch match {
						case MonTypeToMatch.BaseForm => "Base Form"
						case MonTypeToMatch.EvolvedForm => "Natural Evolution"
						case MonTypeToMatch.Neither => "Off"
					  })
					, dt("Experience Group Match")
					, dd(if (settings.expGroupMustMatch) {"Yes"} else {"No"})
					, dt("Natural Evolution Allowed")
					, dd(if (settings.naturalEvoAllowed) {"Yes"} else {"No"})
					, dt("Remaining Evolution Stage Match")
					, dd(if (settings.remainingStageMatch) {"Yes"} else {"No"})
					, dt("Evolution Into Legendary Allowed")
					, dd(if (settings.legendaryAllowed) {"Yes"} else {"No"})
					, dt("Base Stat Total Range")
					, dd(settings.bstMatchFunction.description)
				  )
				, h2("Pokémon List")
				, pokemonListTable(
					  predictions.extantPokemon
					, settings
					, Map.empty
					, predictions.possibleEvosCount
					, predictions.possiblePrevosCount
					, dexNoLinkModifier
				  )
				, h2("Prediction Summary")
				, predictionSummary(predictions.extantPokemon, predictions.possibleEvosCount)
			),
			seedDatas.map{seedData => frag(
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
							(fromNo, methodto) <- seedData.evolutions.to[Seq].sortBy{_._1};
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
					, settings
					, Seq.empty, predictions.possibleEvosCount, predictions.possiblePrevosCount
					, dexNoLinkModifier
				  )
				, h2("Pokémon that multiple things evolve into")
				, pokemonListTable(
					  seedData.multiplePrevos.map{predictions.getPokemon _}
					, settings
					, Seq.empty, predictions.possibleEvosCount, predictions.possiblePrevosCount
					, dexNoLinkModifier
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
					, settings
					, Seq.empty, predictions.possibleEvosCount, predictions.possiblePrevosCount
					, dexNoLinkModifier
				  )
				, h2("Pokémon who eventually evolve into their vanilla final stage")
				, {
					val mons = for (
						startDexNo <- predictions.extantPokemon.map{_.dexNo};
						thisGameFinalEvo <- seedData.finalEvolutions(startDexNo);
						naturalFinalEvo <- NaturalSeedData.finalEvolutions(startDexNo)
									if (thisGameFinalEvo == naturalFinalEvo) 
					) yield { predictions.getPokemon(startDexNo) }
					
					pokemonListTable(mons, settings, Seq.empty, predictions.possibleEvosCount, predictions.possiblePrevosCount, dexNoLinkModifier)
				  }
				, if (settings.typeMatch == MonTypeToMatch.Neither) {frag(
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
							):Seq[ElementalType.Value] = {
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
								(fromNo, toNos) <- seedData.evolutions.to[Seq].sortBy{_._1};
								(method, toNo) <- toNos.to[Seq].sortBy{_._2};
								fromMon <- Seq(predictions.getPokemon(fromNo));
								toMon <- Seq(predictions.getPokemon(toNo));
								typ <- haveSharedType(fromMon, toMon)
							) yield {
								tr(
									  td(dataSort := fromMon.dexNo.toStringPadded)(fromMon.dexNo.toString)
									, td(dataSort := fromMon.name)(fromMon.name)
									, td(dataType := typ.toString.toLowerCase, dataSort := typ.toString)(typ.toString)
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
				, h2("Large evolution chain convergences")
				, ul(
					seedData.families.to[Seq].sortBy{_._1}.map{case (end, members) =>
						if (members.size >= 6) {frag(
							h4(a(href := s"${end}.html", predictions.getPokemon(end).name), " – ", members.size.toString),
							ul(
								members.to[Seq].sorted.map{member => li(member + " " + predictions.getPokemon(member).name)}:_*
							)
						)} else {frag("")}
					}.to[Seq]:_*
				  )
			)}.getOrElse(frag(""))
		)
	}
	
	def sharedPage():scalatags.generic.Frag[Builder,FragT] = {
		val seedDatas = SeedData.runToValue
		val runs = seedDatas.keys.to[Seq]
		val nameHeaders = runs.map{_.shortName}.map{x => th(x)}
		val runColgroups = runs.map(runToColgroup)
		
		def linkToSubpageIfSubpageExists(num:DexNo, name:String):scalatags.generic.Frag[Builder,FragT] = {
			val subpages = Seq("133")
			if (subpages contains num.toString) {
				a(hrefDexNoLinkModifier(num))(name)
			} else {
				name
			}
		}
		
		
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
						, frag( runColgroups:_* )
						, thead(tr(
							  th("From Num")
							, th("From")
							, th("Method")
							, th("To Num")
							, th("To")
							, frag( nameHeaders:_* )
						  ))
						, tbody(frag({
							for (
								prevo <- AllPokemon.apply;
								(method, _) <- seedData.Natural.evolutions.get(prevo.dexNo).getOrElse(Map.empty)
							) yield {
								val evos:Seq[(Run, DexNo)] = {
									for (
										(run, seedData) <- seedDatas.to[Seq];
										prevoData <- seedData.evolutions.get(prevo.dexNo);
										methodData <- prevoData.get(method)
									) yield { (run, methodData) }
								}
								
								val multiGameEvos:Map[DexNo, Seq[Run]] = {
									evos.groupBy{_._2}.mapValues{_.map{_._1}}.filter{_._2.size >= 2}
								}
								
								multiGameEvos.to[Seq].map{case (evoNum, run) =>
									val evoName = AllPokemon.get(evoNum).get.name
									val prevoNum = prevo.dexNo
									val prevoName = prevo.name
									
									tr(
										  td(dataSort := prevoNum.toStringPadded, prevoNum.toString)
										, td(dataSort := prevoName, linkToSubpageIfSubpageExists(prevoNum, prevoName))
										, td(dataSort := method, method)
										, td(dataSort := evoNum.toStringPadded, evoNum.toString)
										, td(dataSort := evoName, linkToSubpageIfSubpageExists(evoNum, evoName))
										, frag( seedDatas.map(_._1).map({x =>
											if (run contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
										  }).to[Seq]:_*)
									)
								}
							}
						  }.flatten:_*))
					  )
					, h2("Repeat Final Evolutions")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "5")
						, frag( runColgroups:_* )
						, thead(tr(
							  th("From Num")
							, th("From")
							, th("→")
							, th("To Num")
							, th("To")
							, frag( nameHeaders:_* )
						  ))
						, tbody(frag({
							AllPokemon.apply.map{prevo =>
								val allFinalEvoNums = seedDatas.values.flatMap{_.finalEvolutions(prevo.dexNo)}.to[SortedSet].to[Seq]
								
								frag({allFinalEvoNums.map{evoNum =>
									val games = seedDatas.filter({_._2.finalEvolutions(prevo.dexNo) contains evoNum}).map(_._1).to[SortedSet]
									if (games.size >= 2) {
										val evoName = AllPokemon.get(evoNum).get.name
										
										tr(
											  td(dataSort := prevo.dexNo.toStringPadded, prevo.dexNo.toString)
											, td(dataSort := prevo.name, linkToSubpageIfSubpageExists(prevo.dexNo, prevo.name))
											, td(dataSort := "→", "→")
											, td(dataSort := evoNum.toStringPadded, evoNum.toString)
											, td(dataSort := evoName, linkToSubpageIfSubpageExists(evoNum, evoName))
											, frag( runs.map{x =>
												if (games contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
											  }.to[Seq]:_*)
										)
									} else {
										frag("")
									}
								}}:_*)
							}
						  }:_*))
					  )
					, h2("Pokémon with multiple prevos multiple times")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "2")
						, frag( runColgroups:_* )
						, thead(tr(
							  th("DexNo")
							, th("Pokémon")
							, frag( nameHeaders:_* )
						  ))
						, tbody(frag({
							AllPokemon.apply.map{mon =>
								val games = seedDatas.filter({_._2.multiplePrevos contains mon.dexNo}).map(_._1).to[SortedSet]
								if (games.size >= 2) {
									val evoNum = mon.dexNo
									val evoName = mon.name
									
									tr(
										  td(dataSort := evoNum.toStringPadded, evoNum.toString)
										, td(dataSort := evoName, linkToSubpageIfSubpageExists(evoNum, evoName))
										, frag( runs.map({x =>
											if (games contains x) {td(dataSort := "0", "✓")} else {td(dataSort := "1", "")}
										  }).to[Seq]:_*)
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
						, frag( runColgroups:_* )
						, thead(tr(
							  th("DexNo")
							, th("Pokémon")
							, frag( nameHeaders:_* )
						  ))
						, tbody(frag({
							AllPokemon.apply.map{mon =>
								val games = runs.filter{run => SeedData.runToValue(run).firstStageMons contains mon.dexNo}
								if (games.size >= 2) {
									val evoNum = mon.dexNo
									val evoName = mon.name
									
									tr(
										  td(dataSort := evoNum.toStringPadded, evoNum.toString)
										, td(dataSort := evoName, linkToSubpageIfSubpageExists(evoNum, evoName))
										, frag( runs.map{x =>
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
	
	def sharedEeveePage(runs:Seq[Runs.Value]):scalatags.generic.Frag[Builder,FragT] = {
		val eeveeDexNo = DexNo.national(133)
		val runTableHeaders = runs.map{_.shortName}.map{x => th(x)}
		val runColGroups = runs.map(runToColgroup)
		
		val methodIcons:Map[String, String] = Map(
			"Used Item [Water Stone]" -> "🌊", // ☂
			"Used Item [Thunder Stone]" -> "💡", // ☈
			"Used Item [Fire Stone]" -> "🔥",
			"Level Up at Morning with Friendship" -> "☀",
			"Level Up at Night with Friendship" -> "🌙",
			"Level Up at Forest" -> "🍂",
			"Level Up at Cold" -> "❄", //⛇. ☃
			"Level Up with 50 Affection + MoveType [Fairy]" -> "❤" //♥
		)
		val methodSorting:Map[String, String] = Map(
			"Used Item [Water Stone]" -> "1",
			"Used Item [Thunder Stone]" -> "2",
			"Used Item [Fire Stone]" -> "3",
			"Level Up at Morning with Friendship" -> "4",
			"Level Up at Night with Friendship" -> "5",
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
						, frag( runColGroups:_* )
						, thead(tr(
							th("To Num"),
							th("To"),
							frag( runTableHeaders:_* )
						  ))
						, tbody(frag({
							val eeveeEvos:Map[DexNo, Map[Run, Seq[String]]] = {
								val a:Seq[(DexNo, Run, String)] = for (
									run <- runs;
									seedData <- SeedData.runToValue.get(run).to[Seq];
									(method, toNo) <- seedData.evolutions(eeveeDexNo)
								) yield {(toNo, run, method)}
								a.groupBy{_._1}.mapValues{_.groupBy{_._2}.mapValues{_.map{_._3}}.map{x => x}}.map{x => x}
							}
							def nameOf(dexNo:DexNo) = AllPokemon.get(dexNo).map{_.name}.getOrElse{"???"}
							
							eeveeEvos.to[Seq].sortBy{_._1}.map{case (toNo, gameMethods) => tr(
								td(dataSort := toNo.toStringPadded, toNo.toString),
								td(dataSort := nameOf(toNo), nameOf(toNo)),
								frag(runs.map{run =>
									val methods = gameMethods.getOrElse(run, Seq.empty)
									val sortStr = {
										val a = methods.map{method => methodSorting.getOrElse(method, "-1")}.mkString
										if (a == "") {"\u007E"} else {a}
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
	
	def sharedFossilPage():scalatags.generic.Frag[Builder,FragT] = {
		val data = Fossils.seedData
		
		frag(htmlDoctype, html(lang := "en-US")(
			  head(
				  title(s"Possible Evolutions - Shared - Fossils")
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
					  h1("Fossils")
					, h2("Revivals")
					, table(`class` := "checktable")(
						  colgroup(colgroupSpan := "2")
						, frag( data.keys.to[Seq].map(runToColgroup):_* )
						, thead(tr(
							th("To Num"),
							th("To"),
							frag( data.map{x => th(x._1.name)}.to[Seq]:_* )
						  ))
						, tbody(frag({
							val dataInvert:Map[DexNo, Map[Run, Seq[Fossils.Value]]] = {
								val a:Seq[(DexNo, Run, Fossils.Value)] = for (
									(gameName, gameData) <- data.to[Seq];
									(fossilName, toNo) <- gameData
								) yield {(toNo, gameName, fossilName)}
								a.groupBy{_._1}.mapValues{_.groupBy{_._2}.mapValues{_.map{_._3}}.map{x => x}}.map{x => x}
							}
							def nameOf(dexNo:DexNo) = AllPokemon.get(dexNo).map{_.name}.getOrElse{"???"}
							
							dataInvert.to[Seq].sortBy{_._1}.map{case (toNo, gameMethods) => tr(
								td(dataSort := toNo.toStringPadded, toNo.toString),
								td(dataSort := nameOf(toNo), nameOf(toNo)),
								frag(data.map{_._1}.to[Seq].map{gameName =>
									val fossils = gameMethods.getOrElse(gameName, Seq.empty)
									val sortStr = {
										val a = fossils.map{fossil => "" + ('A' + fossil.id).toChar}.mkString
										if (a == "") {"\u007E"} else {a}
									}
									val iconStr = fossils.mkString(", ")
									td(dataSort := sortStr, iconStr)
								}:_*)
							)}
						  }:_*))
					  )
				  )
			  )
		))
	}
	
	private[this] val theoreticalPageNoScript:String = "For technical reasons, these calculations are done in the browser, so if scripting is disabled then the page won't work."
	
	def theoreticalFormPage:scalatags.generic.Frag[Builder,FragT] = {
		
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
				  title(s"Possible Evolutions - Theoretical - Form")
				, link(rel := "stylesheet", href := "../style/style.css")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/theoreticalPageForm.js")(" ")
			  )
			, body(
				  header(
					a(href := "../index.html")("Back to Index")
				  )
				, main(
					  p(
						theoreticalPageNoScript
					  )
					, h1("Settings")
					, form(`id` := "theoretical-game-properties", action := "results.html",
						  h2("Baseline Information")
						, options("Generation", "dexNos", Seq(
							  "Gen1" -> "1-151"
							, "Gen2" -> "1-251"
							, "Gen3" -> "1-386"
							, "Gen4" -> "1-493"
							, "Gen5" -> "1-649"
							, "Gen6" -> "1-721"
							, "Gen7" -> ("1-802," + DexNo.alolanDexNos.mkString(","))
							, "Gen7+" -> ("1-807," + DexNo.alolanDexNos.mkString(",") + "," + DexNo.duskRockruff.toString)
						  ))
						, options("Types", "types", Seq(
							  "Normal" -> MonTypeType.Natural.toString
							, "Normal Sans Fairy" -> MonTypeType.NoFairy.toString
							, "Random Platinum" -> MonTypeType.RandPlat.toString
						  ))
						, options("BST Values", "bsts", Seq(
							  "Gen1" -> MonBstType.Gen1.toString
							, "Gen2-5" -> MonBstType.Gen2.toString
							, "Gen6" -> MonBstType.Gen6.toString
							, "Gen7" -> MonBstType.Gen7.toString
						  ))
						, h2("Evolution Restrictions")
						, checkbox("Experience Group Match", "expGroupMatch")
						, checkbox("Natural Evolution Allowed", "naturalEvolutionAllowed")
						, checkbox("Evolution Stages Remaining Match", "stagesRemainingMatch")
						, checkbox("Evolution Into Legendary Allowed", "legendaryAllowed")
						, options("New evolution's type must match", "typeToMatch", Seq(
							  "Nothing" -> MonTypeToMatch.Neither.toString
							, "Base's type" -> MonTypeToMatch.BaseForm.toString
							, "Natural evolution's type" -> MonTypeToMatch.EvolvedForm.toString
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
									, label("5/6 to 6/5 of natural (Like pk3DS before Sep 10 2017)", `for` := "bstdifference_pk")
								  )
								, li(
									  input(`type` := "radio", id := "bstdifference_pk2", name := "bstdifference", value := "pk2")
									, label("10/11 to 11/10 of natural (Like pk3DS after Sep 10 2017)", `for` := "bstdifference_pk2")
								  )
							  	, li(
									  input(`type` := "radio", id := "bstdifference_ur", name := "bstdifference", value := "ur")
									, label("9/10 to 11/10 of natural (like Universal Randomizer)", `for` := "bstdifference_ur")
								  )
								, li(
									  input(`type` := "radio", id := "bstdifference_godt", name := "bstdifference", value := "godt")
									, label("Greater than current (Like GoD tool)", `for` := "bstdifference_godt")
								  )
								, fieldset(
									legend(
										  input(`type` := "radio", id := "bstdifference_custom", name := "bstdifference", value := "custom")
										, label("Custom", `for` := "bstdifference_custom")
									)
									, div(
										  label("Minimum of Natural: ", `for` := "bstdifference_min")
										, input(`type` := "number", name := "bstdifference_min", id := "bstdifference_min", step := 0.1, max := 1, min := 0, value := "0.9")
									  )
									, div(
										  label("Maximum of Natural: ", `for` := "bstdifference_max")
										, input(`type` := "number", name := "bstdifference_max", id := "bstdifference_max", step := 0.1, max := 10, min := 1, value := "1.1")
									  )
								)
							)
						  )
						, h2("Generate")
						, button("Generate", `type` := "submit", id := "generate")
					  )
				  )
			  )
		))
	}
	
	def theoreticalPage:scalatags.generic.Frag[Builder,FragT] = {
		frag(htmlDoctype, html(lang := "en-US")(
			  head(
				  title(s"Possible Evolutions - Theoretical")
				, link(rel := "stylesheet", href := "../style/style.css")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/sectionCollapse.js")(" ")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/tableSort.js")(" ")
				, script(defer := "defer", `type` := "text/javascript", src := "../style/theoreticalPage.js")(" ")
			  )
			, body(
				  header(
					a(href := "../index.html")("Back to Index"),
					" > ",
					a(href := "index.html")("Form")
				  )
				, main(
					  p(
						theoreticalPageNoScript
					  )
				  )
			  )
		))
	}
	
	
	private[this] def monPredictionSection(
			  possible:Seq[Pokemon]
			, settings:RandomizerSettings
			, observed:Iterable[(Runs.Value, DexNo)]
			, possibleEvosCount:DexNo => Int
			, possiblePrevosCount:DexNo => Int
			, resolveDexNo:DexNo => Pokemon
			, dexNoLinkModifier:DexNo => scalatags.generic.Modifier[Builder]
			, seedDatas:Option[SeedData]
	):scalatags.generic.Frag[Builder,FragT] = frag(
		  div(s"Number of candidates: ${possible.size}")
		, seedDatas.map{seedData => frag(
			  h4("Observed")
			, {
				val observedThisGame = observed.filter({x => RandomizerSettings.runToValue.get(x._1) == Option(settings)}).map(_._2).map(resolveDexNo).to[Seq]
				if (observedThisGame.size == 0) {
					p("None")
				} else {
					pokemonListTable(observedThisGame, settings, observed, possibleEvosCount, possiblePrevosCount, dexNoLinkModifier)
				}
			  }
		  )}.getOrElse(frag(""))
		, h4("Candidates")
		, {
			if (possible.size == 0) {
				p("None")
			} else {
				pokemonListTable(possible, settings, observed, possibleEvosCount, possiblePrevosCount, dexNoLinkModifier)
			}
		  }
	)
	
	
	private[this] def pokemonListTable(
			  x:Iterable[Pokemon]
			, settings:RandomizerSettings
			, realEvos:Iterable[(Runs.Value, DexNo)]
			, possibleEvosCount:DexNo => Int
			, possiblePrevosCount:DexNo => Int
			, dexNoLinkModifier:DexNo => scalatags.generic.Modifier[Builder]
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
					.map{(pokemonTableRow(settings, realEvos, possibleEvosCount, possiblePrevosCount, dexNoLinkModifier) _)}
			):_*)
		)
	}
	
	private[this] def predictionSummary(
			  mons:Iterable[Pokemon]
			, possibleEvosCount:DexNo => Int
	):scalatags.generic.Frag[Builder,FragT] = {
		import bundle.svgTags.{attr => _, frag => _, tag => _, modifier => _, _}
		import bundle.svgAttrs.{attr => _, frag => _, tag => _, modifier => _, _}
		
		// key: number of evolutions; value: number of pokemon with that evolution count
		// `- 0` to ignore the half of Pokemon that don't evolve into anything
		val counts:Map[Int, Int] = {
			mons.map{_.dexNo}
				.groupBy(possibleEvosCount)
				.mapValues{_.size}.map{x => x}
				.withDefaultValue(0)
		} - 0
		val maxKey = counts.keySet.max
		val maxValue = counts.values.max
		val countCount = counts.values.sum
		
		val histogramPath = (1 to maxKey).map{k => s"V${counts(k)} h1 "}.mkString("M0,0 ", "", "V0 Z")
		val median = {
			val sortedSeq = counts.to[Seq].flatMap{case (k, v) => Seq.fill(v){k}}.sorted
			assert(sortedSeq.size == countCount)
			if (countCount % 2 == 0) {
				(sortedSeq(countCount / 2) + sortedSeq(countCount / 2 - 1)) / 2.0
			} else {
				sortedSeq((countCount + 1) / 2)
			}
		}
		
		frag(
			h3("Evolutions per Pokémon"),
			// width="calc(100% - 1em)"` is illegal, hence need to put y-axis labels out-of-bounds
			svg(`class` := "histogram", width := "100%", height := "7em", overflow := "visible")(
				svg(x := 0, y := 0, width := "100%", height := "6em", viewBox := s"0 0 $maxKey $maxValue", preserveAspectRatio := "none meet")(
					rect(x := 0, y := 0, width := maxKey, height := maxValue, fill := "#CCC"),
					path(`class` := "data", d := histogramPath, fill := "blue", transform := s"scale(1, -1) translate(0, -$maxValue)"),
				),
				text(x := "50%", y := "7em", fontWeight := "bold", textAnchor := "middle", "Number of Possible Evolutions"),
				text(x := "0%", y := "7em", textAnchor := "start", "1"),
				text(x := "100%", y := "7em", textAnchor := "end", s"${maxKey}"),
				svg(x := "-1", y := "3em", width := "100%", height := "3em", overflow := "visible")(
					// create a new coordinate system to get around the inability to use units in transforms
					text(x := 0, y := 0, fontWeight := "bold", textAnchor := "middle", transform := "rotate(270)", "Count"),
				),
				text(x := "-1px", y := "6em", textAnchor := "end", "0"),
				text(x := "-1px", y := "1em", textAnchor := "end", s"${maxValue}"),
			),
			dl(
				dt("Mean"), dd( counts.map{case (a,b) => a * b}.sum.doubleValue / countCount ),
				dt("Median"), dd( median ),
				dt("Mode"), dd( counts.maxBy{_._2}._1 + " with " + counts.maxBy{_._2}._2 ),
				dt("Range"), dd( counts.keySet.min + " to " + maxKey ),
			),
		)
	}
	
	private[this] def pokemonTableRow(
			settings:RandomizerSettings,
			  realEvos:Iterable[(Runs.Value, DexNo)]
			, possibleEvosCount:DexNo => Int
			, possiblePrevosCount:DexNo => Int
			, dexNoLinkModifier:DexNo => scalatags.generic.Modifier[Builder]
			)(x:Pokemon
	):scalatags.generic.Frag[Builder,FragT] = {
		implicit val typeType:MonTypeType.Value = settings.typeType
		implicit val bstType:MonBstType.Value = settings.bstType
		val game = realEvos.filter({_._2 == x.dexNo}).map(_._1).to[Seq].distinct.sortBy(_.id).map(_.name).mkString("", " ", "")
		
		tr(dataGame := game)(
			  td(dataSort := x.dexNo.toStringPadded)(x.dexNo.toString)
			, td(dataSort := x.name)(
				a(dexNoLinkModifier(x.dexNo))(x.name)
			  )
			, td(dataSort := x.types._1.toString, dataType := x.types._1.toString.toLowerCase)(x.types._1.toString)
			, td(dataSort := x.types._2.toString, dataType := x.types._2.toString.toLowerCase)(x.types._2.toString) 
			, td(dataSort := padStrWithZeros(x.bst))(x.bst.toString)
			, td(dataSort := x.expGrowth.toString)(x.expGrowth.toString)
			, td(dataSort := padStrWithZeros(possibleEvosCount(x.dexNo)))(possibleEvosCount(x.dexNo).toString)
			, td(dataSort := padStrWithZeros(possiblePrevosCount(x.dexNo)))(possiblePrevosCount(x.dexNo).toString)
		)
	}
	
	private[this] def padStrWithZeros(x:Int):String = ("00000" + x).takeRight(5)
}
