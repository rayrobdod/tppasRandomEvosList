package com.rayrobdod.possibleEvolutions
package benchmark

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.State

object PageTemplatesBench {
	
	@State(org.openjdk.jmh.annotations.Scope.Thread)
	class _State {
		val dexnos = (1 to 50).map{DexNo(_)}
		val games = Seq(EvosGame.AlphaSapphire, EvosGame.Platinum, EvosGame.White2)

		val predictors = games.map{game => ((game, new Predictor(game)))}
	}
}

class PageTemplatesBench {
	import PageTemplatesBench._State
	
	@Benchmark
	def perMonPage(state:_State) = {
		val predictor = state.predictors.head
		
		state.dexnos.map{dexno => PageTemplatesText.perMonPage(dexno, predictor._2, predictor._1)}
	}
	
	@Benchmark
	def perGamePage(state:_State) = {
		state.predictors.map{case (a,b) => PageTemplatesText.perGamePage(b, a)}
	}
	
	@Benchmark
	def sharedPage(state:_State) = {
		PageTemplatesText.sharedPage
	}
}
