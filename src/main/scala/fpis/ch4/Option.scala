package fpis.ch4

sealed trait Option[+A] {

  // ex 1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  // ex 1
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  // ex 1
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  // ex 1
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some[B]).getOrElse(ob)

  // ex 1
  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
