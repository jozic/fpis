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

  import fpis.ch9.JSON._

  val spaces = regex("\\w+?".r)

  val openCurlyBrace = regex("{".r)

  val closeCurlyBrace = regex("}".r)

  val openSquareBracket = regex("[".r)

  val closeSquareBracket = regex("]".r)

  val colon = regex(":".r)

  val quote = regex("\"".r)

  val nonQuotes = regex("[^\"]*".r)

  val jString: Parser[JString] = (quote take2 nonQuotes take1 quote).map(JString)

  val jBoolean: Parser[JBool] = regex( """(true|false)""".r).map(s => JBool(s == "true"))

  val jNull: Parser[JNull.type] = regex( """null""".r).map(_ => JNull)

  val jNumber: Parser[JNumber] = regex( """<number regex goes here>""".r).map(s => JNumber(s.toDouble))

  val jField: Parser[(String, JSON)] = (jString take1 spaces take1 colon take1 spaces).map(_.get) ** jValue

  val jObject: Parser[JObject] = (openCurlyBrace take2 jField.many take1 closeCurlyBrace).map(fields => JObject(fields.toMap))

  val jArray: Parser[JArray] = (openSquareBracket take2 jValue.many take1 closeSquareBracket).map(values => JArray(values.toIndexedSeq))

  val jValue: Parser[JSON] = jString | jBoolean | jNull | jNumber | jArray | jObject

}