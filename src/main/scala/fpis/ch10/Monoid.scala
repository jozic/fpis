package fpis.ch10

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoids {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  // ex 1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2

    override def zero = 0
  }

  // ex 1
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2

    override def zero = 1
  }

  // ex 1
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2

    override def zero = false
  }

  // ex 1
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2

    override def zero = true
  }

  // ex 2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]) = a1 orElse a2

    override def zero = None
  }

  // ex 3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2

    override def zero = identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  // ex 5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) {
      case (b, a) => m.op(b, f(a))
    }

  // ex 6
  def foldRightViaFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => f(_, a))(z)

  // ex 7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
    case IndexedSeq() => m.zero
    case IndexedSeq(a) => f(a)
    case _ =>
      val (left, right) = v.splitAt(v.size / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
  }

}
