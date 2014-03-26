package fpis.ch4

object Options {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  // ex 2
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap {
    m => mean(xs.map(x => math.pow(x - m, 2)))
  }

}