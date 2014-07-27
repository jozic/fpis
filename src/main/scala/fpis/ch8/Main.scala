package fpis.ch8

import fpis.ch6.Simple

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

  {
    val (fiveToEleven, _) = Gen.chooseRecursive(5, 12).sample.run(Simple(0))
    assert(fiveToEleven >= 5 && fiveToEleven < 12)
  }
  {
    val (fiveToEleven, _) = Gen.choose(5, 12).sample.run(Simple(0))
    assert(fiveToEleven >= 5 && fiveToEleven < 12)
  }

}
