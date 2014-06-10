package fpis.ch6

object Main extends App {

  import RNG._

  assert(nonNegativeInt(Simple(0))._1 >= 0)

  assert(double(Simple(0))._1 >= 0D)
  assert(double(Simple(0))._1 < 1D)

  def linear(start: Int): RNG = new RNG {
    def nextInt = start -> new RNG {
      def nextInt = (start + 1) -> linear(start + 2)
    }
  }

  assert(ints(5)(linear(1))._1 == List(5, 4, 3, 2, 1))

  // ex 7
  assert(sequence(List(unit(1), unit(2), unit(3)))(Simple(0))._1 == List(1, 2, 3))

}
