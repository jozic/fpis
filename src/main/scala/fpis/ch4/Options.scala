package fpis.ch4

object Options {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  // ex 2
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap {
    m => mean(xs.map(x => math.pow(x - m, 2)))
  }

  // ex 3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    b.flatMap(bb => a.map(aa => f(aa, bb)))


  // ex 4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight(Some(List[A]()): Option[List[A]]) {
    case (Some(aa), Some(l)) => Some(aa :: l)
    case _ => None
  }

}