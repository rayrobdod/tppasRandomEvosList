package com.rayrobdod.possibleEvolutions

import java.time.Instant
import scala.reflect.macros.blackbox.Context
import language.experimental.macros

package object compiledOnMacro {
	
	/** Returns the time that the class calling this macro was compiled */
	def apply:Instant = macro impl
	def impl(c:Context):c.Expr[Instant] = {
		import c.universe._
		val now = Instant.now
		val nowSecs = now.getEpochSecond
		val nowNano = now.getNano
		c.Expr[Instant](q"""java.time.Instant.ofEpochSecond($nowSecs, $nowNano)""")
	}
}
