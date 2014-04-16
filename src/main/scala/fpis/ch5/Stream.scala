package fpis.ch5

import Stream._

sealed abstract class Stream[+A] {

  // ex 6
  lazy val uncons: Option[Cons[A]] = foldRight(None: Option[Cons[A]]) {
    (a, b) => Option(cons(a, b.getOrElse(empty[A])).asInstanceOf[Cons[A]])
  }

  def isEmpty: Boolean = uncons.isEmpty

  // ex 1
  def toList: List[A] = uncons match {
    case None => Nil
    case Some(c) => c.head :: c.tail.toList
  }

  // ex 2
  def take(n: Int): Stream[A] = uncons match {
    case Some(c) if n == 1 => cons(c.head, empty)
    case Some(c) if n > 0 => cons(c.head, c.tail.take(n - 1))
    case _ => empty
  }

  // ex 3
  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some(c) if p(c.head) => cons(c.head, c.tail.takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B

  def foldRight0[B](z: => B)(f: (A, => B) => B): B = uncons match {
    case Some(c) => f(c.head, c.tail.foldRight(z)(f))
    case None => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // ex 4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // ex 5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (a, b) => if (p(a)) cons(a, b) else b
  }

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
  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)(_.uncons.map(c => f(c.head) -> c.tail))

  // ex 13
  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold(n, this) {
    case (nn, _) if nn == 0 => None
    case (nn, s) if nn == 1 => s.uncons.map(c => (c.head, (nn - 1, empty[A])))
    case (nn, s) => s.uncons.map(c => (c.head, (nn - 1, c.tail)))
  }

  // ex 13
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(_.uncons.flatMap {
      case c if p(c.head) => Some(c.head -> c.tail)
      case _ => None
    })

  // ex 13
  def zip[B](other: Stream[B]): Stream[(A, B)] = Stream.unfold((this, other)) {
    case (sa, sb) => for {
      ac <- sa.uncons
      bc <- sb.uncons
    } yield (ac.head -> bc.head) -> (ac.tail -> bc.tail)
  }

  // ex 13
  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, other)) { case (sa, sb) =>
    (sa.uncons, sb.uncons) match {
      case (None, None) => None
      case (aco, bco) =>
        Some((aco.map(_.head) -> bco.map(_.head), aco.fold(empty[A])(_.tail) -> bco.fold(empty[B])(_.tail)))
    }
  }

}

object Empty extends Stream[Nothing] {
  // ex 6
  def foldRight[B](z: => B)(f: (Nothing, => B) => B) = z
}

sealed abstract class Cons[+A] extends Stream[A] {
  def head: A

  def tail: Stream[A]

  // ex 6
  def foldRight[B](z: => B)(f: (A, => B) => B) =
    f(head, tail.foldRight(z)(f))
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = hd
    lazy val tail = tl
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  // ex 8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = new Cons[A] {
    lazy val tail = this

    lazy val head = a
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

  // ex 14 (hard)
  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    if (s eq s2) true
    else s.zipAll(s2).forAll {
      case (Some(a1), Some(a2)) => a1 == a2
      case (None, Some(_)) => false
      case (_, None) => true
    }

}