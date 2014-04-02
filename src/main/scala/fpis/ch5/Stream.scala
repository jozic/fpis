package fpis.ch5

import Stream._

sealed abstract class Stream[+A] {
  def uncons: Option[Cons[A]]

  def isEmpty: Boolean = uncons.isEmpty

  // ex 1
  def toList: List[A] = uncons match {
    case None => Nil
    case Some(c) => c.head :: c.tail.toList
  }

  // ex 2
  def take(n: Int): Stream[A] = {
    def go(i: Int, result: Stream[A] = Empty): Stream[A] = if (i == 0) result
    else this match {
      case c: Cons[A] => cons(c.head, c.tail.take(n - 1))
      case _ => result
    }
    go(n)
  }

  // ex 3
  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some(c) if p(c.head) => cons(c.head, c.tail.takeWhile(p))
    case _ => Empty
  }

}

object Empty extends Stream[Nothing] {
  val uncons = None
}

sealed abstract class Cons[+A] extends Stream[A] {
  def head: A

  def tail: Stream[A]

  val uncons = Some(this)
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = hd
    lazy val tail = tl
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
}