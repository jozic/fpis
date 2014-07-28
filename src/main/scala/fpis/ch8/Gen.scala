package fpis.ch8

import fpis.ch6.{RNG, State}

import scala.annotation.tailrec


case class Gen[A](sample: State[RNG, A])

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

}
