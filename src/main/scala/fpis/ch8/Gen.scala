package fpis.ch8

import fpis.ch6.{RNG, State}

import scala.annotation.tailrec


case class Gen[A](sample: State[RNG, A]) {

  // ex 6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  // ex 6
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap {
    sz => Gen.listOfN(sz, this)
  }
}

object Gen {
  // ex 4
  def chooseRecursive(start: Int, stopExclusive: Int): Gen[Int] = {

    @tailrec
    def inRange(r: RNG): (Int, RNG) = {
      val (i, r2) = r.nextInt
      if (i >= start && i < stopExclusive) (i, r2)
      else inRange(r2)
    }
    Gen(State(inRange))
  }

  // ex 4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))

  // ex 5
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  // ex 5
  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(_ % 2 == 0))

  // ex 5
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  // playing
  def option[A](gen: Gen[A]): Gen[Option[A]] = Gen(boolean.sample.flatMap {
    b => if (b) gen.sample.map(s => Option(s)) else State.unit(Option.empty[A])
  })

  // ex 7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap { b =>
    if (b) g1 else g2
  }


}
