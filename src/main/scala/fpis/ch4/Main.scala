package fpis.ch4

object Main extends App {

  def opt[A](a: A): Option[A] = if (a == null) None else Some(a)


  assert(opt("1").map(_.toInt) == Some(1))
  assert(opt(null: String).map(_.toInt) == None)

  assert(opt("1").flatMap(x => Some(x.toInt)) == Some(1))
  assert(opt("1").flatMap(_ => None) == None)
  assert(opt(null: String).flatMap(x => Some(x.toInt)) == None)

  assert(opt("").getOrElse("a") == "")
  assert(opt(null: String).getOrElse("a") == "a")

  assert(opt("").orElse(Some("a")) == Some(""))
  assert(opt(null: String).orElse(Some("a")) == Some("a"))

  assert(opt("").filter(_.isEmpty) == Some(""))
  assert(opt("q").filter(_.isEmpty) == None)

}