package com.rayrobdod.possibleEvolutions
package benchmark

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.State

object PageTemplatesBench {
	
	@State(org.openjdk.jmh.annotations.Scope.Thread)
	class _State {
		val accessOrder = (1 to 50).map{DexNo(_)}

		val data = {
			ListOfPokemon.fromFiles(new java.io.File("C:/Users/Raymond/Documents/Programming/HTML-JS/RandomizedEvolutions/src/main/data/"))
		}
	}
}

class PageTemplatesBench {
	import PageTemplatesBench._State
	
	@Benchmark
	def perMonPage(state:_State) = {
		implicit val config = EvosGame.White2
		
		state.accessOrder.map{dexno => PageTemplates.perMonPage(dexno, state.data)}
	}
}