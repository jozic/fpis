package fpis

package object ch3 {

  import fpis.ch3.List._

  object ex1 {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this will match
      case Cons(h, t) => h + sum(t)
      case _ => 101
    } // returns 3

  }

}