package com.rayrobdod.possibleEvolutions
package benchmark

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.State

object PageTemplatesBench {
	
	@State(org.openjdk.jmh.annotations.Scope.Thread)
	class _State {
		val dexnos = (1 to 50).map{DexNo(_)}
		val games = Seq(EvosGame.AlphaSapphire, EvosGame.Platinum, EvosGame.White2)

		val data = {
			ListOfPokemon.fromFiles(new java.io.File("src/main/data/"))
		}
	}
}

class PageTemplatesBench {
	import PageTemplatesBench._State
	
	@Benchmark
	def perMonPage(state:_State) = {
		implicit val config = EvosGame.White2
		
		state.dexnos.map{dexno => PageTemplates.perMonPage(dexno, state.data)}
	}
	
	@Benchmark
	def perGamePage(state:_State) = {
		state.games.map{game => PageTemplates.perGamePage(game, state.data)}
	}
	
	@Benchmark
	def sharedPage(state:_State) = {
		PageTemplates.sharedPage(state.data)
	}
}
