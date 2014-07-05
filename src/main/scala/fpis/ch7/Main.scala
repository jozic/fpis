package fpis.ch7

import java.util.concurrent.Executors

object Main extends App {

  import fpis.ch7.Par._

  val es = Executors.newFixedThreadPool(16)

  def eql[A](p: Par[A], a: A) = equal(es)(unit(a), p)

  assert(eql(lazyUnit(5), 5))

  assert(eql(sum2(Vector()), 0))

  assert(eql(sum2(Vector(4)), 4))

  assert(eql(sum2(Vector(1, 2, 3, 4, 5, 6)), 21))

  assert(eql(count(List()), 0))

  assert(eql(count(List(
    "please please, count",
    "the words correctly",
    "will you do it for me?")
  ), 12))

  es.shutdownNow()

}
