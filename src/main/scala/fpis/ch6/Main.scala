package fpis.ch6

object Main extends App {

  assert(RNG.nonNegativeInt(Simple(0))._1 >= 0)

  assert(RNG.double(Simple(0))._1 >= 0D)
  assert(RNG.double(Simple(0))._1 < 1D)

}
