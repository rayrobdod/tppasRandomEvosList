package com.rayrobdod.possibleEvolutions

import sbt._
import sbt.Keys._
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable.Buffer
import scala.language.implicitConversions

/**
 * Reads the sbt project's `README.md`, takes the first section of that file,
 * and generates a set of scalatags instructions that would recreate that text
 */
object GenPrologue extends AutoPlugin {
	private final class TagsScalaSourceBuilder {
		val parts = Buffer.empty[String]
		
		def render:String = parts.mkString("scalatags.Text.all.frag(\n\t\t", ",\n\t\t", "\n\t\t)")
	}
	private object TagsScalaSource extends scalatags.generic.Bundle[TagsScalaSourceBuilder, String, String] {
		final case class RawFrag(x:String) extends TagsScalaSource.Modifier {
			def applyTo(t:TagsScalaSourceBuilder):Unit = {
				t.parts += ("scalatags.Text.RawFrag(\"\"\"" + x + "\"\"\")")
			}
		}
		final case class StringFrag(x:String) extends TagsScalaSource.Modifier {
			def applyTo(t:TagsScalaSourceBuilder):Unit = {
				t.parts += ("scalatags.Text.StringFrag(\"\"\"" + x + "\"\"\")")
			}
		}
		object RawFrag extends scalatags.Companion[TagsScalaSource.RawFrag]
		object StringFrag extends scalatags.Companion[TagsScalaSource.StringFrag]
		
		trait Shared {
			final class ConcreteHtmlTag[T <: String](
					val tag:String, void:Boolean, ns:scalatags.generic.Namespace, val modifiers:List[Seq[Modifier]]
			) extends scalatags.generic.TypedTag[TagsScalaSourceBuilder, T, String] {
				type Self = ConcreteHtmlTag[T]
				
				def apply(xs:Modifier*):Self = {new ConcreteHtmlTag(this.tag, void, ns, xs :: this.modifiers) }
				def applyTo(t:TagsScalaSourceBuilder):Unit = { t.parts += this.render }
				def render:T = {
					val nsStr = s"""new MyNamespace("${ns.uri}")"""
					val prefix = s"""scalatags.Text.tags.makeAbstractTypedTag("$tag", $void, $nsStr)(\n\t\t"""
					val suffix = "\n\t\t)"
					val middle = {
						val builder = new TagsScalaSourceBuilder
						for (a <- this.modifiers.reverse; b <- a) { b.applyTo(builder) } 
						builder.parts
					}
					
					middle.mkString(prefix, ",\n\t\t", suffix).asInstanceOf[T]
				}
			}
			
			implicit def SeqFrag[A](xs:Seq[A])(implicit ev:A => TagsScalaSource.Frag):TagsScalaSource.Frag = {
				this.GeneratorFrag(geny.Generator.from(xs))(ev)
			}
			implicit def GeneratorFrag[A](xs: geny.Generator[A])(implicit ev: A => TagsScalaSource.Frag):TagsScalaSource.Frag = new TagsScalaSource.Frag {
				val xs2 = xs.map(ev)
				def applyTo(t:TagsScalaSourceBuilder):Unit = {
					xs2.foreach{_.applyTo(t)}
				}
				def render:String = {
					val b = new TagsScalaSourceBuilder
					this.applyTo(b)
					b.render
				}
			}
			implicit def UnitFrag(u: Unit):TagsScalaSource.Frag = new TagsScalaSource.Frag{
				def applyTo(t:TagsScalaSourceBuilder):Unit = {}
				def render:String = ""
			}
			implicit protected[this] def stringAttrX: scalatags.generic.AttrValue[TagsScalaSourceBuilder,String] = ???
			implicit protected[this] def stringPixelStyleX: scalatags.generic.PixelStyleValue[TagsScalaSourceBuilder,String] = ???
			implicit protected[this] def stringStyleX: scalatags.generic.StyleValue[TagsScalaSourceBuilder,String] = ???
			
			def tag(tag:String, void:Boolean = false):ConcreteHtmlTag[String] = makeAbstractTypedTag(tag, void)
			
			def makeAbstractTypedTag[T <: String](tag:String, void:Boolean = false):ConcreteHtmlTag[T] = {
				makeAbstractTypedTag(tag, void, scalatags.generic.Namespace.htmlNamespaceConfig)
			}
			
			def makeAbstractTypedTag[T <: String](
				  tag:String
				, void:Boolean
				, ns:scalatags.generic.Namespace
			):ConcreteHtmlTag[T] = {
				new ConcreteHtmlTag(tag, void, ns, Nil)
			}
		}
		
		trait SharedTags extends Shared {
			val a:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("a")
			val area:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("area", true)
			val audio:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("audio")
			val b:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("b", true)
			val base:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("base")
			val blockquote:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("blockquote")
			val body:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("body")
			val br:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("br", true)
			val button:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("button")
			val canvas:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("canvas")
			val caption:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("caption")
			val cite:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("cite")
			val code:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("code")
			val col:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("col", true)
			val colgroup:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("colgroup")
			val datalist:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("datalist")
			val dd:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("dd")
			val del:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("del")
			val div:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("div")
			val dl:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("dl")
			val dt:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("dt")
			val em:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("em")
			val embed:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("embed", true)
			val fieldset:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("fieldset")
			val figcaption:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("figcaption")
			val figure:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("figure")
			val footer:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("footer")
			val form:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("form")
			val h1:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("h1")
			val h2:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("h2")
			val h3:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("h3")
			val h4:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("h4")
			val h5:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("h5")
			val h6:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("h6")
			val head:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("head")
			val header:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("header")
			val hr:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("hr", true)
			val html:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("html")
			val i:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("i")
			val iframe:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("iframe")
			val img:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("img", true)
			val input:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("input", true)
			val ins:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("ins")
			val label:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("label")
			val legend:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("legend")
			val li:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("li")
			val link:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("link", true)
			val map:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("map")
			val meta:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("meta", true)
			val `object`:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("object")
			val ol:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("ol")
			val optgroup:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("optgroup")
			val option:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("option")
			val p:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("p")
			val param:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("param", true)
			val pre:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("pre")
			val s:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("s")
			val script:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("script")
			val select:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("select")
			val small:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("small")
			val source:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("source", true)
			val span:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("span")
			val strong:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("strong")
			val sub:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("sub")
			val sup:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("sup")
			val table:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("table")
			val tbody:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("tbody")
			val td:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("td")
			val textarea:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("textarea")
			val tfoot:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("tfoot")
			val th:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("th")
			val thead:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("thead")
			val tr:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("tr")
			val track:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("track", true)
			val u:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("u")
			val ul:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("ul")
			val video:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("video")
			val wbr:ConcreteHtmlTag[String] = makeAbstractTypedTag[String]("wbr", true)
		}
		
		trait SharedImplicits {
			implicit def ClsModifier(s:scalatags.stylesheet.Cls):TagsScalaSource.Modifier = ???
			implicit def StyleFrag(s:scalatags.generic.StylePair[TagsScalaSourceBuilder, _]):scalatags.stylesheet.StyleSheetFrag = ???
			def genericAttr[T]:TagsScalaSource.AttrValue[T] = new TagsScalaSource.AttrValue[T] {
				def apply(t:TagsScalaSourceBuilder, a:Attr, v:T):Unit = {
					t.parts += (
						"new scalatags.Text.AttrPair(" +
							s"""new scalatags.Text.Attr("${a.name}", ${a.namespace}, ${a.raw}), """ +
							s""" "$v", """ +
							"scalatags.Text.implicits.genericAttr" +
						")"
					)
				}
			}
			def genericPixelStyle[T](implicit ev:TagsScalaSource.StyleValue[T]):TagsScalaSource.PixelStyleValue[T] = null
			def genericPixelStylePx[T](implicit ev:TagsScalaSource.StyleValue[String]):TagsScalaSource.PixelStyleValue[T] = null
			def genericStyle[T]:TagsScalaSource.StyleValue[T] = new TagsScalaSource.StyleValue[T]{
				def apply(t:TagsScalaSourceBuilder, a:Style, v:T):Unit = {
					t.parts += (
						"new scalatags.Text.StylePair(" +
							s"""new scalatags.Text.Style("${a.jsName}", "${a.cssName}"),""" +
							s""" "$v", """ +
							"scalatags.Text.implicits.genericStyle" +
						")"
					)
				}
			}
			def raw(s:String):TagsScalaSource.RawFrag = RawFrag.apply(s)
			implicit def stringFrag(v:String):TagsScalaSource.Frag = ???
			
			type RawFrag = TagsScalaSource.RawFrag
			protected[this] val RawFrag = TagsScalaSource.RawFrag
			type StringFrag = TagsScalaSource.StringFrag
			protected[this] val StringFrag = TagsScalaSource.StringFrag
		}
		
		object all extends TagsScalaSource.Attrs with TagsScalaSource.Styles
				with TagsScalaSource.Tags with scalatags.DataConverters
				with TagsScalaSource.Util with scalatags.generic.Aggregate[TagsScalaSourceBuilder,String,String]
				with TagsScalaSource.SharedImplicits with TagsScalaSource.SharedTags
		object attrs extends TagsScalaSource.Attrs with TagsScalaSource.Shared
		object implicits extends scalatags.generic.Aggregate[TagsScalaSourceBuilder, String, String] with scalatags.DataConverters
				with TagsScalaSource.SharedImplicits
		lazy val short:TagsScalaSource.AbstractShort with TagsScalaSource.Tags with scalatags.DataConverters with scalatags.generic.Aggregate[TagsScalaSourceBuilder,String,String] = ???
		object styles extends TagsScalaSource.Styles with TagsScalaSource.Shared
		object styles2 extends TagsScalaSource.Styles2 with TagsScalaSource.Shared
		object svgAttrs extends TagsScalaSource.SvgAttrs with TagsScalaSource.Shared
		lazy val svgTags:TagsScalaSource.SvgTags = ???
		object tags extends TagsScalaSource.Tags with TagsScalaSource.SharedTags
		lazy val tags2:TagsScalaSource.Tags2 = ???
	}
	
	private def readPrologue(readmemdFile:File):TagsScalaSource.Frag = {
		import TagsScalaSource.StringFrag
		import TagsScalaSource.all.stringAttr
		import scala.collection.JavaConverters._
		val containsLink = """([^\[]*)\[([ é\w]+)\]\(([\w\:\/\.\-]+)\)(.*)""".r
		
		val emptyParagraph = TagsScalaSource.tags.p
		
		val lines = Files.readAllLines(readmemdFile.toPath, UTF_8)
		val usedLines = lines.asScala.dropWhile{!_.startsWith("#")}.drop(1)
				.takeWhile{!_.startsWith("##")}
				.dropWhile{_ == "\n"}
				.reverse.dropWhile{_ == "\n"}.reverse
		TagsScalaSource.tags.frag(usedLines.foldLeft(Seq(emptyParagraph)){(folding, line) => line match {
			case "" if folding.last == emptyParagraph => folding
			case "" => folding :+ emptyParagraph
			case containsLink(before, label, href, after) => {
				folding.init :+ (folding.last(StringFrag(" "), StringFrag(before),
						TagsScalaSource.tags.a(TagsScalaSource.attrs.href := href)(StringFrag(label)),
						StringFrag(after)))
			}
			case _ => {
				folding.init :+ (folding.last(StringFrag(" "), StringFrag(line)))
			}
		}}:_*)
	}
	
	object autoImport {
		val genPrologue = taskKey[Seq[File]]("Generate a scala file whose contents mirror README.md")
	}
	import autoImport.genPrologue
	
	override lazy val projectSettings = Seq(
		target in genPrologue in Compile := {
			(sourceManaged in Compile).value / "IndexPrologue.scala"
		},
		genPrologue in Compile := {
			val data = readPrologue(baseDirectory.value / ".." / "README.md")
			val outFile = (target in genPrologue in Compile).value
			
			val prefix = """|package com.rayrobdod.possibleEvolutions
				|
				|object IndexPrologue {
				|	final class MyNamespace(val uri:String) extends scalatags.generic.Namespace
				|	def apply:scalatags.Text.Frag = {
				|		""".stripMargin
			val suffix = """|
				|	}
				|}
				|""".stripMargin
			
			sbt.IO.createDirectory(outFile.getParentFile)
			sbt.IO.write(outFile, prefix ++ data.render ++ suffix, UTF_8, false)
			Seq(outFile)
		},
		sourceGenerators in Compile += (genPrologue in Compile).taskValue
	)
	
}
