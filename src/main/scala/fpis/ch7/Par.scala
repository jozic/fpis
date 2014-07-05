package fpis.ch7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A])(s: ExecutorService): Future[A] = a(s)

  // ex 1
  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => {
    val f1: Future[A] = p1(es)
    val f2: Future[B] = p2(es)
    UnitFuture(f(f1.get, f2.get))
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call() = a(es).get
  })

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }


}
