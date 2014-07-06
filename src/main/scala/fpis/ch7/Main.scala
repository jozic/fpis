package fpis.ch7

import java.util.concurrent.{Executors, TimeUnit, TimeoutException}

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


  val es1 = Executors.newSingleThreadExecutor()

  try {
    assert(sum2(Vector(1, 2, 3, 4, 5, 6))(es1).get(1, TimeUnit.SECONDS) == 21)
    assert(1 == 0, "never happens")
  } catch {
    case _: TimeoutException => println("alas")
  }

  es1.shutdownNow()

}
