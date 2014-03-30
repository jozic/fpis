package fpis.ch4

object Main extends App {

  def some[A](a: A): Option[A] = Some(a)

  def none[A]: Option[A] = None

  assert(some("1").map(_.toInt) == Some(1))
  assert(none[String].map(_.toInt) == None)

  assert(some("1").flatMap(x => Some(x.toInt)) == Some(1))
  assert(some("1").flatMap(_ => None) == None)
  assert(none[String].flatMap(x => Some(x.toInt)) == None)

  assert(some("").getOrElse("a") == "")
  assert(none[String].getOrElse("a") == "a")

  assert(some("").orElse(some("a")) == Some(""))
  assert(none[String].orElse(some("a")) == Some("a"))

  assert(some("").filter(_.isEmpty) == Some(""))
  assert(some("q").filter(_.isEmpty) == None)

  assert(Options.mean(Seq(1, 2, 3, 4)) == Some(2.5))
  assert(Options.variance(Seq(1, 2, 3, 4)) == Some(1.25))

  assert(Options.map2(some(1), some("2"))(_ + _.toInt) == Some(3))
  assert(Options.map2(none[Int], Some("2"))(_ + _.toInt) == None)
  assert(Options.map2(some(1), none[String])(_ + _.toInt) == None)


  assert(Options.sequence(Nil) == Some(Nil))
  assert(Options.sequence(List(some(1))) == Some(List(1)))
  assert(Options.sequence(List(some(1), some(2), some(3))) == Some(List(1, 2, 3)))
  assert(Options.sequence(List(some(1), some(2), some(3), none[Int])) == None)

  assert(Options.traverse0(Nil)(identity) == Some(Nil))
  assert(Options.traverse0(List(1))(a => some(a.toString)) == Some(List("1")))
  assert(Options.traverse0(List(1, 2, 3))(a => some(a.toString)) == Some(List("1", "2", "3")))
  assert(Options.traverse0(List(1, 2, 3))(a => if (a == 3) none[String] else some(a.toString)) == None)

  assert(Options.traverse(Nil)(identity) == Some(Nil))
  assert(Options.traverse(List(1))(a => some(a.toString)) == Some(List("1")))
  assert(Options.traverse(List(1, 2, 3))(a => some(a.toString)) == Some(List("1", "2", "3")))
  assert(Options.traverse(List(1, 2, 3))(a => if (a == 3) none[String] else some(a.toString)) == None)

  assert(Options.sequenceViaTraverse(Nil) == Some(Nil))
  assert(Options.sequenceViaTraverse(List(some(1))) == Some(List(1)))
  assert(Options.sequenceViaTraverse(List(some(1), some(2), some(3))) == Some(List(1, 2, 3)))
  assert(Options.sequenceViaTraverse(List(some(1), some(2), some(3), none[Int])) == None)


}