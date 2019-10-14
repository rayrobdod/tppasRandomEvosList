package com.rayrobdod.possibleEvolutions

import sbt._
import sbtcrossproject.Platform

object SharedCrossType extends sbtcrossproject.CrossPlugin.autoImport.CrossType {
	def projectDir(crossBase: File, platformIdentifier:String): File =
		crossBase / ("." + platformIdentifier)
	def projectDir(crossBase: File, platform:Platform): File =
		crossBase / ("." + platform.identifier)
	def sharedSrcDir(projectBase:File, conf:String):Option[File] =
		Option(projectBase / ".." / "src" / conf / "scala")
}
