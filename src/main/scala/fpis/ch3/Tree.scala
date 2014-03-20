package fpis.ch3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // ex 25
  def size(t: Tree[_]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  // ex 26
  def maximum(t: Tree[Int]): Int = {
    def go(tr: Tree[Int], res: Int = Int.MinValue): Int = tr match {
      case Leaf(x) => math.max(res, x)
      case Branch(l, r) => math.max(math.max(go(l), go(r)), res)
    }
    go(t)
  }

  // ex 27
  def depth(t: Tree[_]): Int = {
    def go(tr: Tree[_], res: Int = 0): Int = tr match {
      case Leaf(_) => 1 + res
      case Branch(l, r) => 1 + math.max(go(l, res), go(r, res))
    }
    go(t)
  }

  // ex 28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // ex 29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  // ex 29
  def sizeFold(t: Tree[_]): Int = fold(t)(_ => 1)((l1, l2) => l1 + l2 + 1)

  // ex 29
  def maximumFold(t: Tree[Int]): Int = fold(t)(identity)((m1, m2) => math.max(m1, m2))

  // ex 29
  def depthFold(t: Tree[_]): Int = fold(t)(_ => 1)((d1, d2) => math.max(d1, d2) + 1)

  // ex 29
  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch.apply[B])

}