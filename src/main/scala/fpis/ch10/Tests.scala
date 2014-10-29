package fpis.ch10

import fpis.ch8.{Gen, Prop}

object Tests extends App {

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(Gen.triple(gen)) { case (a1, a2, a3) =>
      m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)
    } &&
      Prop.forAll(gen) { a =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a
      }

  Prop.run(monoidLaws(Monoids.booleanOr, Gen.boolean), label = "booleanOr")
  Prop.run(monoidLaws(Monoids.booleanAnd, Gen.boolean), label = "booleanAnd")
  Prop.run(monoidLaws(Monoids.listMonoid[Boolean], Gen.listOfN(3, Gen.boolean)), label = "listMonoid")
  Prop.run(monoidLaws(Monoids.intAddition, Gen.choose(0, 10)), label = "intAddition")
  Prop.run(monoidLaws(Monoids.intMultiplication, Gen.choose(0, 10)), label = "intMultiplication")
  Prop.run(monoidLaws(Monoids.optionMonoid[Int], Gen.option(Gen.choose(5, 10))), label = "optionMonoid")

  Prop.run(Prop.forAll(Gen.listOfN(10, Gen.choose(0, 100))) { l =>
    Monoids.foldMapV(l.toIndexedSeq, Monoids.intAddition)(identity) == l.sum
  }, label = "foldMapV")

  Prop.run(Prop.forAll(Gen.listOfN(10, Gen.choose(0, 100))) { l =>
    Monoids.foldRightViaFoldMap(l)(0)(_ + _) == l.sum
  }, label = "foldRightViaFoldMap")

  Prop.run(Prop.forAll(Gen.listOfN(10, Gen.choose(0, 100))) { l =>
    Monoids.foldLeftViaFoldMap(l)(0)(_ + _) == l.sum
  }, label = "foldLeftViaFoldMap")

}
