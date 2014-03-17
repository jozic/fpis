package fpis.ch2

object Main extends App {

  assert(formatResult("fib", 1, fib) == "The fib of 1 is 0.")
  assert(formatResult("fib", 2, fib) == "The fib of 2 is 1.")
  assert(formatResult("fib", 3, fib) == "The fib of 3 is 1.")
  assert(formatResult("fib", 4, fib) == "The fib of 4 is 2.")
  assert(formatResult("fib", 5, fib) == "The fib of 5 is 3.")
  assert(formatResult("fib", 6, fib) == "The fib of 6 is 5.")
  assert(formatResult("fib", 7, fib) == "The fib of 7 is 8.")

  assert(isSorted[Int](Array(5, 4, 3, 2, 1), _ > _))
  assert(isSorted[String](Array("5", "4", "3", "2", "1"), _ > _))
  assert(!isSorted[Double](Array(3d, 5d), _ > _))
}