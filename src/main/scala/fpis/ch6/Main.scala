package fpis.ch6

object Main extends App {

  assert(RNG.nonNegativeInt(Simple(0))._1 >= 0)

  assert(RNG.double(Simple(0))._1 >= 0D)
  assert(RNG.double(Simple(0))._1 < 1D)

  def linear(start: Int): RNG = new RNG {
    def nextInt = start -> new RNG {
      def nextInt = (start + 1) -> linear(start + 2)
    }
  }

  println(RNG.ints(5)(linear(1))._1)
  assert(RNG.ints(5)(linear(1))._1 == List(5, 4, 3, 2, 1))


}
