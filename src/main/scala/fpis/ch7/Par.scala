package fpis.ch7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class BinaryFuture[A, B, C](f1: Future[A], f2: Future[B])
                                          (f: (A, B) => C) extends Future[C] {
    private val futures = List(f1, f2)

    override def isDone = futures.forall(_.isDone)

    override def cancel(mayInterruptIfRunning: Boolean) = futures.exists(_.cancel(mayInterruptIfRunning))

    override def isCancelled = futures.exists(_.isCancelled)

    override def get() = get(Long.MaxValue, TimeUnit.DAYS)

    override def get(timeout: Long, unit: TimeUnit): C = {
      val timeoutInMillis = unit.convert(timeout, TimeUnit.MILLISECONDS)
      val deadline = timeoutInMillis + System.currentTimeMillis()
      val a = f1.get(timeoutInMillis, TimeUnit.MILLISECONDS)
      val b = f2.get(deadline - System.currentTimeMillis(), TimeUnit.MILLISECONDS)
      f(a, b)
    }
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

  // ex 3 (hard, optional)
  def map22[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => {
    val f1: Future[A] = p1(es)
    val f2: Future[B] = p2(es)
    BinaryFuture(f1, f2)(f)
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call() = a(es).get
  })

  // ex 4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // ex 5
  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(l.map(a => asyncF(f)(a)))
  }

  // ex 6
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
    sequence(l.withFilter(f).map(asyncF[A, A](identity)(_)))
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map22(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def parFold[A, B](seq: IndexedSeq[A])(ifEmpty: => B, f: A => B, rd: (B, B) => B): Par[B] =
    if (seq.size <= 1)
      Par.unit(seq.headOption.fold(ifEmpty)(f))
    else {
      val (l, r) = seq.splitAt(seq.length / 2)
      Par.map22(Par.fork(parFold(l)(ifEmpty, f, rd)), Par.fork(parFold(r)(ifEmpty, f, rd)))(rd)
    }

  def parReduce[A](seq: IndexedSeq[A])(ifEmpty: => A, rd: (A, A) => A): Par[A] =
    parFold(seq)(ifEmpty, identity[A], rd)

  def sum2(ints: IndexedSeq[Int]): Par[Int] = parReduce(ints)(0, _ + _)

  def max(ints: IndexedSeq[Int]): Par[Int] = parReduce(ints)(???, math.max)

  def count(paragraphs: List[String]): Par[Int] =
    parFold(paragraphs.toIndexedSeq)(0, _.split(" ").size, _ + _)


  def equal[A](es: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(es).get == p2(es).get

}
