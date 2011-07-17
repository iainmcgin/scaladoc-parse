import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import LinkParser._

class LinkParserSuite extends FunSuite with ShouldMatchers {
	
  def parseTest[T](description : String, parser : Parser[T], input : String, expected : T) = 
    test(description + ": " + input) {
      val result = parseAll(parser, input)
      assert(result.successful)
      result.get should be (expected)
    }
  
  def el(entityPath : List[PathElement], description : String) = new EntityLink(entityPath, description)
  def wl(url : String, description : String) = new WebLink(url, description)

  def a(name : String) = new Ambiguous(name)
  def m(name : String, params : List[List[PathElement]]) = new Method(name, params)
  def o(name : String) = new Object(name)
  def ref(parts : PathElement*) : List[PathElement] = List(parts : _*)

  val predef = List(a("scala"), o("Predef"))
  val assumeMethod = m("assume", List(ref(a("scala"),a("Boolean"))))
  // main tests of link rule

  parseTest("parse link to Predef", link, "[[scala.Predef$]]", el(predef, ""))

  parseTest("parse link to member of Predef", link, 
    "[[scala.Predef$.AnyRef]]", 
    el(predef :+ a("AnyRef"), ""))
  
  
  parseTest("parse link to function member of Predef", link,
    "[[scala.Predef$.assume(scala.Boolean)]]",
    el(predef :+ assumeMethod, ""))

  parseTest("parse link with escaped segment", link,
    "[[my.`Annoying Object!`$.isAmazing()]]",
    el(List(a("my"), o("Annoying Object!"), m("isAmazing", Nil)), "")
    )

  parseTest("parse web link", link,
    "[[http://www.scala-lang.org/ The Scala Website]]",
    wl("http://www.scala-lang.org/", "The Scala Website")
    )

  // smaller scale tests for subrules
  parseTest("parse unescaped name2", name, "Predef", "Predef")
  parseTest("parse escaped name", name, "`Name with Spaces`", "Name with Spaces")
  parseTest("parse single ambiguousPart", ambiguousPart, "scala", a("scala"))
  parseTest("parse single objectPart", objectPart, "Predef$", o("Predef"))
  parseTest("parse single escaped objectPart", objectPart, "`Object with Spaces`$", o("Object with Spaces"))
  parseTest("parse single elem paramList", paramList, "Int", List(ref(a("Int"))))
  parseTest("parse standalone url", url, "http://www.scala-lang.org/", "http://www.scala-lang.org/")
  parseTest("parse url with anchor", url, "http://www.example.com/test.html#section5", "http://www.example.com/test.html#section5")
  
  parseTest("parse multi elem paramList with spacing", paramList, 
    "Int, String ,\t Float", 
    List(ref(a("Int")), ref(a("String")), ref(a("Float")))
    )
  
  parseTest("parse single methodPart with two params", 
    methodPart, 
    "doStuff(Int,Int)", 
    m("doStuff", List(ref(a("Int")), ref(a("Int"))))
    )
  parseTest("parse id tail", entityIdTail, ".a.b.c", List(a("a"), a("b"), a("c")))


}