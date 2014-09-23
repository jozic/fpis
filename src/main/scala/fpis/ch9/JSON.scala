package fpis.ch9

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

}

trait JsonParsers[ParseError, Parser[+ _]] extends Parsers[ParseError, Parser] {

  import JSON._

  def spaces = regex("\\w+?".r)

  def jString: Parser[JString] = regex( """".*"""".r).map(JString)

  def jBoolean: Parser[JBool] = regex( """(true|false)""".r).map(s => JBool(s == "true"))

  def jNull: Parser[JNull.type] = regex( """null""".r).map(_ => JNull)

  def jNumber: Parser[JNumber] = regex( """<number regex goes here>""".r).map(s => JNumber(s.toDouble))

  def jValue: Parser[JSON] = jString | jBoolean | jNull | jNumber

  def jField = (jString ** spaces ** regex(":".r) ** spaces ** jValue).map{ case ((jString, _)}

  def jObject: Parser[JObject] =


  val parser: Parser[JSON] =

}