package fpis.ch8

import fpis.ch6.{RNG, Simple}

trait Prop {
  def check: Boolean

  // ex 3
  def &&(p: Prop): Prop = new Prop {
    override def check = Prop.this.check && p.check
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

}
