package fpis.ch3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString = s"${head.toString} :: ${tail.toString}"
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // ex 2
  def tail[A](l: List[A]): List[A] = drop(l, 1)

  // ex 3
  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => List(newHead)
    case Cons(x, xs) => Cons(newHead, xs)
  }

  //ex 4
  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec def go(i: Int, ls: List[A]): List[A] =
      if (i == n) ls
      else ls match {
        case Nil => ls
        case Cons(x, xs) => go(i + 1, xs)
      }
    go(0, l)
  }

  //ex 5
  @tailrec def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  //ex 6
  def init[A](l: List[A]): List[A] = {
    def go(xs: List[A], ys: List[A]): List[A] = ys match {
      case Nil => throw new RuntimeException("Init on Nil")
      case Cons(x, Nil) => xs
      case Cons(x, xxs) => Cons(x, go(xs, xxs))
    }
    go(Nil, l)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Double]) = foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)

  // ex 9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, l) => l + 1)

  // ex 10
  @tailrec def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // ex 11
  def sum3(l: List[Double]) = foldLeft(l, 0.0)(_ + _)

  // ex 11
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  // ex 11
  def length3[A](l: List[A]): Int = foldLeft(l, 0)((l, _) => l + 1)

  // ex 12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((r, a) => Cons(a, r))

  // ex 13 (hard)
  def foldLeftInFoldRight1[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    val h = (a: A, g: B => B) => (b: B) => g(f(b, a))
    val foldLeftFunction: B => B = foldRight(l, identity[B] _)(h)
    foldLeftFunction(z)
  }

  def foldLeftInFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, identity[B] _)((a, g) => b => g(f(b, a)))(z)

  // ex 13 (hard)
  def foldRightInFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, identity[B] _)((g, a) => b => g(f(a, b)))(z)

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // ex 14
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons.apply)

  // ex 15 (hard)
  def concat[A](lls: List[List[A]]): List[A] = foldRight(lls, List[A]())(append2)

  // ex 16
  def plus1(l: List[Int]): List[Int] = foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  // ex 17
  def doublesToStrings(l: List[Double]): List[String] =
    foldRight(l, List[String]())((a, b) => Cons(a.toString, b))

  // ex 18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((x, xs) => Cons(f(x), xs))

  // ex 18
  def plus1_2(l: List[Int]): List[Int] = map(l)(_ + 1)

  // ex 18
  def doublesToStrings_2(l: List[Double]): List[String] = map(l)(_.toString)

  // ex 19
  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, List[A]())((x, xs) => if (p(x)) Cons(x, xs) else xs)

  // ex 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, List[B]())((x, xs) => append(f(x), xs))

  // ex 21
  def filterInFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else Nil)

  def zip[A, B](l1: List[A], l2: List[B]): List[(A, B)] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons((x, y), zip(xs, ys))
    }

  // ex 22
  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = foldRight(zip(l1, l2), List[Int]()) {
    case ((x, y), l) => Cons(x + y, l)
  }

  // ex 23
  def zipMap[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    foldRight(zip(l1, l2), List[C]()) {
      case ((a, b), l) => Cons(f(a, b), l)
    }

  // ex 23
  def zipMap2[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipMap2(as, bs)(f))
    }

  // ex 24 (hard)
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    @tailrec def has(l1: List[A], l2: List[A], result: Boolean, started: Boolean): Boolean = (l1, l2) match {
      case (_, Nil) => result
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) if x == y => has(xs, ys, true, true)
      case (Cons(x, xs), Cons(y, ys)) if started => false
      case (Cons(x, xs), Cons(y, ys)) => has(xs, l2, false, false)
    }
    has(l, sub, true, false)
  }


}
