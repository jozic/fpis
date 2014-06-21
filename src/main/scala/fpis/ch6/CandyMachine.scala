package fpis.ch6

// ext 11 (hard, optional)
object CandyMachine {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  type MachineState = State[Machine, (Int, Int)]

  case class Machine(locked: Boolean, candies: Int, coins: Int) {

    def input(i: Input): Machine =
      if (candies == 0) this
      else (locked, i) match {
        case (true, Coin) => copy(coins = coins + 1, locked = false)
        case (false, Turn) => copy(candies = candies - 1, locked = true)
        case _ => this
      }

    def stats = candies -> coins
  }

  def simulateMachine(inputs: List[Input]): MachineState = State { machine =>
    val resultMachine = inputs.foldLeft(machine) {
      case (m, input) => m.input(input)
    }
    resultMachine.stats -> resultMachine
  }


  def main(args: Array[String]) {
    val ((candies, coins), _) = simulateMachine(
      List(Coin, Coin, Turn, Turn, Coin, Turn, Turn, Coin, Turn, Coin, Coin, Turn, Coin, Coin)
    ).run(Machine(true, 5, 10))
    assert(candies == 1, s"has $candies candies")
    assert(coins == 15, s"has $coins coins")
  }

}
