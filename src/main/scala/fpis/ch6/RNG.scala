package fpis.ch6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

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

  // ex 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = RNG.double(rng1)
    (i, d) -> rng2
  }

  // ex 3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), nRng) = intDouble(rng)
    (d, i) -> nRng
  }

  // ex 3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = RNG.double(rng)
    val (d2, rng2) = RNG.double(rng1)
    val (d3, rng3) = RNG.double(rng2)
    (d1, d2, d3) -> rng3
  }

  // ex 4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, r: RNG, acc: List[Int] = Nil): (List[Int], RNG) = {
      if (c <= 0) acc -> r
      else {
        val (i, r2) = r.nextInt
        go(c - 1, r2, i :: acc)
      }
    }
    go(count, rng)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  //ex 5
  def doubleViaMap: Rand[Double] = map[Int, Double](nonNegativeInt) { i =>
    i.toDouble / Int.MaxValue + 1
  }

  //ex 6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    f(a, b) -> rng
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(nonNegativeInt, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, nonNegativeInt)

  // ex 7 (hard)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((List.empty[A], rng)) {
      case (rand, (acc, anRng)) =>
        val (a, newRng) = rand(anRng)
        (a :: acc) -> newRng
    }
  }

  // ex 7 (hard)
  def sequenceViaMap2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (rand, accRand) =>
      map2(rand, accRand)(_ :: _)
    }

  // ex 8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  // ex 8
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  // ex 9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }

  // ex 9
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

  // ex 9
  def map2ViaFlatMapAndMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
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
