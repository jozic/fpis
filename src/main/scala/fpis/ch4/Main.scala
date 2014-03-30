package fpis.ch4

import Option._
import Either._

object Main extends App {

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


  assert(right[String, Int](1).map(_ + 1) == Right(2))
  assert(left[String, Int]("no").map(_ + 1) == Left("no"))

  assert(right(1).flatMap(x => Right(x + 1)) == Right(2))
  assert(right("1").flatMap(_ => Left("no")) == Left("no"))
  assert(left[String, Int]("no").flatMap(x => Right(x + 1)) == Left("no"))

  assert(right(1).orElse(right(0)) == Right(1))
  assert(left[String, Int]("no").orElse(right(0)) == Right(0))

  assert(right[String, Int](1).map2(right[Int, String]("2"))(_ + _.toInt) == Right(3))
  assert(left[String, Int]("no").map2(right[Int, String]("2"))(_ + _.toInt) == Left("no"))
  assert(right[String, Int](1).map2(left[Int, String](0))(_ + _.toInt) == Left(0))


}