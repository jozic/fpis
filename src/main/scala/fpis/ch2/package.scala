package fpis

import scala.annotation.tailrec


package object ch2 {

  // ex 1
  def fib(n: Int): Int = {
    @tailrec def go(x: Int, a: Int, b: Int): Int = x match {
      case _ if x == n => a + b
      case _ => go(x + 1, b, a + b)
    }
    if (n == 1) 0 else if (n == 2) 1 else go(3, 0, 1)
  }


  // ex 2
  @tailrec def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    if (as.length < 2) true
    else gt(as(0), as(1)) && isSorted(as.drop(1), gt)

  // ex 3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)


  // ex 4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // ex 5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, n * acc)
    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

}
