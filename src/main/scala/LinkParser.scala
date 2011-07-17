import scala.util.parsing.combinator._

abstract class PathElement
case class Ambiguous(name : String) extends PathElement
case class Object(name : String) extends PathElement
case class Method(name : String, args : List[List[PathElement]]) extends PathElement

abstract sealed class Link
case class EntityLink(entity : List[PathElement], description : String) extends Link
case class WebLink(url : String, description : String) extends Link

object LinkParser extends RegexParsers {

  type EntityPath = List[PathElement]

  override def skipWhitespace = false

  def parseLink(input : String) : ParseResult[Link] =
    parseAll(link, input)

  def link : Parser[Link] = "[[" ~> entityOrWeb ~ descriptionSuffix <~ "]]" ^^ extractLink

  def extractLink(linkParts : ~[Either[EntityPath,String],String]) : Link = {
    linkParts._1 match {
      case Left(entityPath) => new EntityLink(entityPath, linkParts._2)
      case Right(url) => new WebLink(url, linkParts._2)
    }
  }

  def entityOrWeb : Parser[Either[EntityPath,String]] = 
    (
      url ^^ { u => Right(u)}
    | entityId ^^ { e => Left(e) }
    )

  def descriptionSuffix : Parser[String] = (("\\s+".r ~> "[^\\]]*".r)?) ^^ { descOpt => descOpt getOrElse "" }

  /*
  def idOrLink = 
    (
      entityId
    | htmlLink
    )
  */

  def entityId : Parser[EntityPath] = idPart ~ entityIdTail ^^ { e => e._1 :: e._2 }
  def entityIdTail : Parser[EntityPath] = ('.' ~> idPart)*

  // XXX: methods should only be on end
  def idPart : Parser[PathElement] = methodPart | objectPart | ambiguousPart

  def methodPart : Parser[PathElement] = (name <~ '(') ~ (paramList ?) <~ ')' ^^ {m => new Method(m._1, m._2 getOrElse Nil) }
  def paramList  : Parser[List[EntityPath]] = entityId ~ paramListTail ^^ { p => p._1 :: p._2 }
  def paramListTail : Parser[List[EntityPath]] = ("""\s*,\s*""".r ~> entityId) *

  def objectPart : Parser[PathElement] = name <~ '$' ^^ (new Object(_))
  def ambiguousPart : Parser[PathElement] = name ^^ (new Ambiguous(_))

  def name : Parser[String] = 
    (
      '`' ~> "([^`])+".r <~ '`' // escaped form
    | """[^\s$`\.\(\)\[\],]+""".r // unescaped form, with special character restrictions
    )

  def myFunction(x : Int => Boolean) = x(1)

  def description = """[^\]]+""".r
    
  /* url matching regular expression from Daring fireball:
   * http://daringfireball.net/2010/07/improved_regex_for_matching_urls
   */ 
  val url : Parser[String] = 
    """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
}