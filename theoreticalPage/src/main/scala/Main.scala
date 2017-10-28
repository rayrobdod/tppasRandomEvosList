package com.rayrobdod.possibleEvolutions

import scala.collection.immutable.Seq
import org.scalajs.dom.document
import scalatags.JsDom.implicits._

object TheoreticalPage {
	private[this] val seedDatas = Seq(evolutionData.Natural)
	
	
	def main(args:Array[String]):Unit = {
		document.addEventListener("DOMContentLoaded", {(x:Any) =>
			document.getElementById("generate").addEventListener("click", generateGamePage _)
		})
	}
	
	def generateGamePage(x:Any):Unit = {
		val mainElem = document.getElementsByTagName("main").apply(0)
		
		// Remove elements previously added by this script
		while (isNotForm(mainElem.lastChild)) {
			mainElem.removeChild(mainElem.lastChild)
		}
		
		/* I can't decide whether the form should be disabled or not upon the first submit
		document.getElementsByTagName("input").toScala.foreach{_ match {
			case e:org.scalajs.dom.raw.Element => e.setAttribute("disabled", "disabled")
		}}
		document.getElementById("generate").setAttribute("disabled", "disabled")
		*/
		
		mainElem.appendChild(
			scalatags.JsDom.tags.h1("Results").render
		)
		
		val game = getEvosGameFromForm()
		implicit val config:EvosGame.Value = game
		
		val predictor = new Predictor(game)
		
		mainElem.appendChild(
			PageTemplatesJsDom.pokemonListTable(
				  x = AllPokemon.apply.filter{_.exists}
				, realEvos = Seq.empty
				, predictor.possibleEvosCount _
				, predictor.possiblePrevosCount _
				, {dexNo => scalatags.JsDom.tags.modifier(scalatags.JsDom.attrs.href := "#", new AddClickListenerModifier(generateMonPage(dexNo) _))}
			).render
		)
		
		SortableTableFunction.makeTablesSortable()
	}
	
	def generateMonPage(monNo:DexNo)(x:Any):Unit = {
		val mainElem = document.getElementsByTagName("main").apply(0)
		
		// Remove elements previously added by this script
		while (isNotForm(mainElem.lastChild)) {
			mainElem.removeChild(mainElem.lastChild)
		}
		
		val monName = AllPokemon.get(monNo).get.name
		mainElem.appendChild(
			scalatags.JsDom.tags.h1(s"Results - $monName").render
		)
		mainElem.appendChild(
			scalatags.JsDom.tags.a(scalatags.JsDom.attrs.href := "#", new AddClickListenerModifier(generateGamePage _), "Back to Game").render
		)
		
		val game = getEvosGameFromForm()
		
		val predictor = new Predictor(game)
		
		mainElem.appendChild(
			PageTemplatesJsDom.perMonMain(
				  monNo = monNo
				, predictions = predictor
				, game = game
				, seedDatas = seedDatas
				, {dexNo => scalatags.JsDom.tags.modifier(scalatags.JsDom.attrs.href := "#", new AddClickListenerModifier(generateMonPage(dexNo) _))}
			).render
		)
		
		SortableTableFunction.makeTablesSortable()
	}
	
	private[this] class AddClickListenerModifier(f:Function[Any, Unit]) extends scalatags.JsDom.Modifier {
		def applyTo(t:org.scalajs.dom.Element):Unit = {
			t.addEventListener("click", f)
		}
	}
	
	private[this] def getEvosGameFromForm():EvosGame.Value = {
		EvosGame.Custom(
			  maxKnownDexno = {
			  	document.querySelectorAll("input[name=\"generation\"]")
					.toScala
					.filter(isCheckboxChecked _)
					.headOption
					.map{_.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement].value}
					.map{id => DexNo(id.toInt)}
					.getOrElse(DexNo(802))  
			  }
			, bstType = {
				document.querySelectorAll("input[name=\"monBstType\"]")
					.toScala
					.filter(isCheckboxChecked _)
					.headOption
					.map{_.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement].value}
					.map{id => MonBstType.apply(id.toInt)}
					.getOrElse(MonBstType.Gen7)
			  }
			, typeType = {
				document.querySelectorAll("input[name=\"monTypeType\"]")
					.toScala
					.filter(isCheckboxChecked _)
					.headOption
					.map{_.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement].value}
					.map{id => MonTypeType.apply(id.toInt)}
					.getOrElse(MonTypeType.Natural)
			  }
			, monToMatch = {
				document.querySelectorAll("input[name=\"monTypeToMatch\"]")
					.toScala
					.filter(isCheckboxChecked _)
					.headOption
					.map{_.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement].value}
					.map{id => MonTypeToMatch.apply(id.toInt)}
					.getOrElse(MonTypeToMatch.Neither)
			  }
			, bstMatchFunction = {
				document.querySelectorAll("input[name=\"bstdifference\"]")
					.toScala
					.filter(isCheckboxChecked _)
					.headOption
					.map{_.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement].value}
					.map{_ match {
						case "any" => BstMatchFunction.`Any`
						case "pk" => BstMatchFunction.Pk3ds
						case "ur" => BstMatchFunction.UniversalRandomizer
						case "custom" => {
							val min = document.getElementById("bstdifference_min").asInstanceOf[org.scalajs.dom.raw.HTMLInputElement].value.toDouble
							val max = document.getElementById("bstdifference_max").asInstanceOf[org.scalajs.dom.raw.HTMLInputElement].value.toDouble
							BstMatchFunction.Custom(min, max)
						}
						case _ => BstMatchFunction.`Any`
					}}
					.getOrElse(BstMatchFunction.Any)
			  }
			, expGroupMustMatch = {
				isCheckboxChecked(document.getElementById("expGroup"))
			  }
			, naturalEvoAllowed = {
				isCheckboxChecked(document.getElementById("naturalEvolution"))
			  }
		)
	}
	
	private[this] def isNotForm(x:org.scalajs.dom.raw.Node):Boolean = x match {
		case e:org.scalajs.dom.raw.Element => ! (e.tagName equalsIgnoreCase "form")
		case null => false
		case _ => true
	}
	
	private[this] def isCheckboxChecked(x:org.scalajs.dom.raw.Node):Boolean = x match {
		case e:org.scalajs.dom.raw.HTMLInputElement => e.checked
		case _ => false
	}
	
	private[this] implicit class NodeListToScalaSeq(coll:org.scalajs.dom.raw.NodeList) {
		def toScala:scala.collection.immutable.Seq[org.scalajs.dom.raw.Node] = {
			(0 until coll.length).map{idx => coll(idx)}
		}
	}
	
	@scala.scalajs.js.native
	@scala.scalajs.js.annotation.JSGlobalScope
	object SortableTableFunction extends scala.scalajs.js.Object {
		def makeTablesSortable():Unit = scala.scalajs.js.native
	}
	
}
