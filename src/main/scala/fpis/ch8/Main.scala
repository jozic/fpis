package fpis.ch8

import fpis.ch6.{RNG, Simple}
import fpis.ch8.Prop.{FailedCase, SuccessCount, TestCases}


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (TestCases, RNG) => Result) {

  // ex 3
  //  def &&(p: Prop): Prop = new Prop {
  //    override def check = Prop.this.check && p.check
  //  }
  //  def check: Result


  // ex 9
  def &&(p: Prop): Prop = Prop {
    (n, rng) => run(n, rng) match {
      case Passed => p.run(n, rng)
      case r => r
    }
  }

  // ex 9
  def ||(p: Prop): Prop = Prop {
    (n, rng) => run(n, rng) match {
      case f: Falsified => p.run(n, rng)
      case r => r
    }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => Gen.randomStream(as, n)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage }\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n") }"


}


object Main extends App {

  //ex 1
  //sum: List[Int] => Int
  // forAll(list) { l =>
  //    sum(l) == l.foldLeft(0)(_+_)
  //    sum(l) == sum(l.reverse)
  // }
  // forAll(sameElList) { l =>
  //  sum(l) == l.headOption.getOrElse(0) * l.size
  //}

  implicit val rng = Simple(0)

  def generate[A](g: Gen[A])(implicit r: RNG) = g.sample.run(r)

  {
    val (fiveToEleven, _) = generate(Gen.choose(5, 12))
    assert(fiveToEleven >= 5 && fiveToEleven < 12)
  }

  val (xs, _) = generate(Gen.listOfN[Boolean](23, Gen.boolean))
  assert(xs.size == 23)

  def assertPropHolds(p: Prop): Unit = assert(!p.run(10, Simple(0)).isFalsified)

  def assertPropDoesnHold(p: Prop): Unit = assert(p.run(10, Simple(0)).isFalsified)

  assertPropDoesnHold(Prop.forAll(Gen.unit(5))(_ > 10) && Prop.forAll(Gen.unit(3))(_ < 2))
  assertPropDoesnHold(Prop.forAll(Gen.unit(5))(_ > 10) && Prop.forAll(Gen.unit(3))(_ < 5))
  assertPropDoesnHold(Prop.forAll(Gen.unit(5))(_ > 3) && Prop.forAll(Gen.unit(3))(_ < 2))
  assertPropHolds(Prop.forAll(Gen.unit(5))(_ > 3) && Prop.forAll(Gen.unit(3))(_ < 5))


  assertPropDoesnHold(Prop.forAll(Gen.unit(5))(_ > 10) || Prop.forAll(Gen.unit(3))(_ < 2))
  assertPropHolds(Prop.forAll(Gen.unit(5))(_ > 10) || Prop.forAll(Gen.unit(3))(_ < 5))
  assertPropHolds(Prop.forAll(Gen.unit(5))(_ > 3) || Prop.forAll(Gen.unit(3))(_ < 2))
  assertPropHolds(Prop.forAll(Gen.unit(5))(_ > 3) || Prop.forAll(Gen.unit(3))(_ < 5))

}
