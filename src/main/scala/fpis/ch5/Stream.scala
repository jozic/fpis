package fpis.ch5

import Stream._

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {

  def isEmpty = this eq Empty

  // ex 1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  // ex 2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons[A](h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // ex 3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      val head = h()
      if (p(head)) cons(head, t().takeWhile(p)) else Empty
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // ex 4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // ex 5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (a, b) => if (p(a)) cons(a, b) else empty
  }

  //ex 6 (hard)
  // TODO: new exercise - re-implement

  // ex 7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, s) => cons(f(a), s))

  // ex 7
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (a, s) => if (p(a)) cons(a, s) else s
  }

  //ex 7
  def append[B >: A](another: => Stream[B]): Stream[B] = foldRight(another)((a, s) => cons(a, s))

  // ex 7
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, s) => f(a).append(s))

  // ex 13
  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some(f(h()) -> t())
    case _ => None
  }

  // ex 13
  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold(n, this) {
    case (nn, Cons(h, t)) if nn == 1 => Some(h(), (nn - 1, empty[A]))
    case (nn, Cons(h, t)) if nn > 0 => Some(h(), (nn - 1, t()))
    case _ => None
  }

  // ex 13
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) =>
        lazy val head = h()
        if (p(head)) Some(head -> t()) else None
      case _ => None
    }

  // ex 13
  def zip[B](other: Stream[B]): Stream[(A, B)] = Stream.unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1() -> h2()) -> (t1() -> t2()))
    case _ => None
  }

  // ex 13
  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, other)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some((Some(h()) -> Option.empty[B], t() -> empty[B]))
    case (Empty, Cons(h, t)) => Some((Option.empty[A] -> Some(h()), empty[A] -> t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))
  }
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // ex 8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = {
    lazy val cons: Stream[A] = new Cons[A](() => a, () => cons)
    cons
  }

  // ex 9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // ex 10
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  // ex 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => Empty
  }

  // ex 12
  def constantViaUnfold[A](a: A): Stream[A] = unfold(null)(_ => Some(a, null))

  // ex 12
  def onesViaUnfold: Stream[Int] = constantViaUnfold(1)

  // ex 12
  def fromViaUnfold(n: Int): Stream[Int] = unfold[Int, Int](1)(a => Some(a, a + 1))

  // ex 12
  def fibsViaUnfold: Stream[Int] = cons(0, unfold((0, 1)) {
    case (a, b) => Some(b, (b, a + b))
  })

  //  ex 14 (hard)
  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    if (s eq s2) true
    else s.zipAll(s2).forAll {
      case (Some(a1), Some(a2)) => a1 == a2
      case (None, Some(_)) => false
      case (_, None) => true
    }

}