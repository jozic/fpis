package fpis.ch8

import fpis.ch5.Stream
import fpis.ch6.{RNG, Simple}
import fpis.ch8.Prop.{MaxSize, FailedCase, SuccessCount, TestCases}


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

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // ex 3
  //  def &&(p: Prop): Prop = new Prop {
  //    override def check = Prop.this.check && p.check
  //  }
  //  def check: Result


  // ex 9
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case r => r
    }
  }

  // ex 9
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case f: Falsified => p.run(max, n, rng)
      case r => r
    }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](as: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng) { rng => Some(as.sample.run(rng)) }


  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage }\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n") }"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }


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

  def assertPropHolds(p: Prop): Unit = assert(!p.run(10, 10, Simple(0)).isFalsified)

  def assertPropDoesnHold(p: Prop): Unit = assert(p.run(10, 10, Simple(0)).isFalsified)

  assertPropDoesnHold(Prop.forAll(Gen.unit(5))(_ > 10) && Prop.forAll(Gen.unit(3))(_ < 2))
  assertPropDoesnHold(Prop.forAll(Gen.unit(5))(_ > 10) && Prop.forAll(Gen.unit(3))(_ < 5))
  assertPropDoesnHold(Prop.forAll(Gen.unit(5))(_ > 3) && Prop.forAll(Gen.unit(3))(_ < 2))
  assertPropHolds(Prop.forAll(Gen.unit(5))(_ > 3) && Prop.forAll(Gen.unit(3))(_ < 5))


  assertPropDoesnHold(Prop.forAll(Gen.unit(5))(_ > 10) || Prop.forAll(Gen.unit(3))(_ < 2))
  assertPropHolds(Prop.forAll(Gen.unit(5))(_ > 10) || Prop.forAll(Gen.unit(3))(_ < 5))
  assertPropHolds(Prop.forAll(Gen.unit(5))(_ > 3) || Prop.forAll(Gen.unit(3))(_ < 2))
  assertPropHolds(Prop.forAll(Gen.unit(5))(_ > 3) || Prop.forAll(Gen.unit(3))(_ < 5))


  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  Prop.run(maxProp)

  // ex 14
  val sortListProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
    val sorted = ns.sorted
    sorted.head == ns.min &&
      sorted.last == ns.max &&
      !ns.exists(!sorted.contains(_))
  }
  Prop.run(sortListProp)


}
