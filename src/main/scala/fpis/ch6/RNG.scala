package fpis.ch6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  // ex 1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (res, newRNG) = rng.nextInt
    if (res == Int.MinValue) nonNegativeInt(newRNG)
    else math.abs(res) -> newRNG
  }

  // ex 2
  def double(rng: RNG): (Double, RNG) = {
    nonNegativeInt(rng) match {
      case (Int.MaxValue, newRNG) => double(newRNG)
      case (i, newRNG) => (i.toDouble / Int.MaxValue, newRNG)
    }
  }
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
