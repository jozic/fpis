package fpis.ch4

sealed trait Either[+E, +A] {

  // ex 6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  // ex 6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = transform(f)(Left[EE])

  // ex 6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = transform(Right[B])(_ => b)

  // ex 6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)

  def transform[EE, B](rf: A => Either[EE, B])(lf: E => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => rf(a)
    case Left(e) => lf(e)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def left[E, A](e: E): Either[E, A] = Left(e)

  def right[E, A](a: A): Either[E, A] = Right(a)

  // ex 7
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = traverse(a)(identity)

  // ex 7
  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a.foldRight(right[E, List[B]](Nil)) {
      case (aa, e) => f(aa).map2(e)(_ :: _)
    }

}
