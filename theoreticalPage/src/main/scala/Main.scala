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
		val monNoOpt:Option[DexNo] = params.get("dexno").map{DexNo.valueOf _}
		val game = getEvosGameFromQueryString(params)
		val predictor = new Predictor(game)
		
		val gamePageUrl = window.location.pathname + mapToQueryString(params - "dexno")
		val monPageUrlFun = {dexNo:DexNo => window.location.pathname + mapToQueryString(params + (("dexno" -> dexNo.toString)))}
		
		monNoOpt.foreach{_ =>
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
					PageTemplatesJsDom.perGameMain(
						  predictions = predictor
						, game = game
						, {dexNo => scalatags.JsDom.tags.modifier(scalatags.JsDom.attrs.href := monPageUrlFun(dexNo))}
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
			  knownDexnos = {
				params.get("dexNos")
					.map{s => java.net.URLDecoder.decode(s, "UTF-8")}
					.map{DexNo.seqValueOf _}
					.getOrElse(AllPokemon.apply.map{_.dexNo})
			  }
			, bstType = {
				params.get("bsts")
					.map{name => MonBstType.withName(name)}
					.getOrElse(MonBstType.Gen7)
			  }
			, typeType = {
				params.get("types")
					.map{name => MonTypeType.withName(name)}
					.getOrElse(MonTypeType.Natural)
			  }
			, monToMatch = {
				params.get("typeToMatch")
					.map{name => MonTypeToMatch.withName(name)}
					.getOrElse(MonTypeToMatch.Neither)
			  }
			, bstMatchFunction = {
				params.get("bstdifference")
					.map{_ match {
						case "any" => BstMatchFunction.`Any`
						case "pk" => BstMatchFunction.Pk3ds
						case "pk2" => BstMatchFunction.Pk3ds_2
						case "ur" => BstMatchFunction.UniversalRandomizer
						case "godt" => BstMatchFunction.GoDTool
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
				params.get("expGroupMatch").nonEmpty
			  }
			, naturalEvoAllowed = {
				params.get("naturalEvolutionAllowed").nonEmpty
			  }
			, remainingStageMatch = {
				params.get("stagesRemainingMatch").nonEmpty
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
