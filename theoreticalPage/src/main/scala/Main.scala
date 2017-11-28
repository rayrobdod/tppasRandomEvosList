package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.Seq
import org.scalajs.dom.document
import org.scalajs.dom.window
import scalatags.JsDom.implicits._

object TheoreticalPage {
	private[this] val seedDatas = Seq(evolutionData.Natural)
	
	
	def main(args:Array[String]):Unit = {
		document.addEventListener("DOMContentLoaded", generatePage _)
	}
	
	def generatePage(event:Any):Unit = {
		val mainElem = document.getElementsByTagName("main").apply(0)
		val navElem = document.getElementsByTagName("header").apply(0)
		
		while (mainElem.hasChildNodes) {
			mainElem.removeChild(mainElem.lastChild)
		}
		
		val params = getQueryString()
		val monNoOpt:Option[DexNo] = params.get("dexno").map{x => DexNo(x.toInt)}
		val game = getEvosGameFromQueryString(params)
		implicit val config:EvosGame.Value = game
		val predictor = new Predictor(game)
		
		val gamePageUrl = window.location.pathname + mapToQueryString(params - "dexno")
		val monPageUrlFun = {dexNo:DexNo => window.location.pathname + mapToQueryString(params + (("dexno" -> dexNo.toString)))}
		
		monNoOpt.foreach{monNo =>
			navElem.appendChild(
				scalatags.JsDom.tags.span(" > ").render
			)
			navElem.appendChild(
				scalatags.JsDom.tags.a("Game")(scalatags.JsDom.attrs.href := gamePageUrl).render
			)
		}
		
		mainElem.appendChild(
			scalatags.JsDom.tags.h1("Theoretical" + monNoOpt.flatMap{AllPokemon.get _}.map{x => " - " + x.name}.getOrElse("")).render
		)
		
		monNoOpt match {
			case None => {
				mainElem.appendChild(
					scalatags.JsDom.tags.h2("List of Pokémon").render
				)
				mainElem.appendChild(
					PageTemplatesJsDom.pokemonListTable(
						  x = AllPokemon.apply.filter{_.exists}
						, realEvos = Seq.empty
						, predictor.possibleEvosCount _
						, predictor.possiblePrevosCount _
						, {dexNo => scalatags.JsDom.tags.modifier(scalatags.JsDom.attrs.href := monPageUrlFun(dexNo))}
					).render
				)
				mainElem.appendChild(
					scalatags.JsDom.tags.h2("Prediction Summary").render
				)
				mainElem.appendChild(
					PageTemplatesJsDom.predictionSummary(
						  mons = AllPokemon.apply.filter{_.exists}
						, possibleEvosCount = predictor.possibleEvosCount _
					).render
				)
			}
			case Some(monNo) => {
				mainElem.appendChild(
					PageTemplatesJsDom.perMonMain(
						  monNo = monNo
						, predictions = predictor
						, game = game
						, seedDatas = seedDatas
						, {dexNo => scalatags.JsDom.tags.modifier(scalatags.JsDom.attrs.href := monPageUrlFun(dexNo))}
					).render
				)
			}
			
		}
		
		SortableTableFunction.makeTablesSortable()
	}
	
	
	private[this] def getEvosGameFromQueryString(params:Map[String, String]):EvosGame.Value = {
		EvosGame.Custom(
			  maxKnownDexno = {
				params.get("maxDexNo")
					.map{id => DexNo(id.toInt)}
					.getOrElse(DexNo.maxPlusOne)
			  }
			, bstType = {
				params.get("monBstType")
					.map{id => MonBstType.apply(id.toInt)}
					.getOrElse(MonBstType.Gen7)
			  }
			, typeType = {
				params.get("monTypeType")
					.map{id => MonTypeType.apply(id.toInt)}
					.getOrElse(MonTypeType.Natural)
			  }
			, monToMatch = {
				params.get("monTypeToMatch")
					.map{id => MonTypeToMatch.apply(id.toInt)}
					.getOrElse(MonTypeToMatch.Neither)
			  }
			, bstMatchFunction = {
				params.get("bstdifference")
					.map{_ match {
						case "any" => BstMatchFunction.`Any`
						case "pk" => BstMatchFunction.Pk3ds
						case "ur" => BstMatchFunction.UniversalRandomizer
						case "custom" => {
							val min = params.get("bstdifference_min").map{_.toDouble}.getOrElse(0d)
							val max = params.get("bstdifference_max").map{_.toDouble}.getOrElse(10d)
							BstMatchFunction.Custom(min, max)
						}
						case _ => BstMatchFunction.`Any`
					}}
					.getOrElse(BstMatchFunction.Any)
			  }
			, expGroupMustMatch = {
				params.get("expGroup")
					.map{x => true}
					.getOrElse(false)
			  }
			, naturalEvoAllowed = {
				params.get("naturalEvolution")
					.map{x => true}
					.getOrElse(false)
			  }
		)
	}
	
	private[this] def getQueryString():Map[String, String] = {
		window.location.search.substring(1).split("&").map{param => 
			val split = param.split("=")
			split match {
				case Array() => (("", ""))
				case Array(k) => ((k, k))
				case Array(k, v) => ((k, v))
				case x:Array[String] => ((x.head, x.tail.mkString("=")))
			}
		}.toMap
	}
	
	private[this] def mapToQueryString(x:Map[String, String]):String = {
		x.map{x => s"${x._1}=${x._2}"}.mkString("?", "&", "")
	}
	
	@scala.scalajs.js.native
	@scala.scalajs.js.annotation.JSGlobalScope
	object SortableTableFunction extends scala.scalajs.js.Object {
		def makeTablesSortable():Unit = scala.scalajs.js.native
	}
	
}
