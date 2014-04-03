package fpis.ch5

object Main extends App {

  assert(Stream.empty.toList == Nil)
  assert(Stream(1).toList == List(1))
  assert(Stream(1, 2, 3).toList == List(1, 2, 3))

  assert(Stream.empty.take(0).toList == Nil)
  assert(Stream.empty.take(10).toList == Nil)
  assert(Stream(1).take(0).toList == Nil)
  assert(Stream(1).take(1).toList == List(1))
  assert(Stream(1).take(10).toList == List(1))
  assert(Stream(1, 2, 3).take(2).toList == List(1, 2))

  assert(Stream.empty[Int].takeWhile(_ < 3).toList == Nil)
  assert(Stream(1).takeWhile(_ > 3).toList == Nil)
  assert(Stream(1).takeWhile(_ < 3).toList == List(1))
  assert(Stream(1, 2, 3).takeWhile(_ < 3).toList == List(1, 2))

  assert(Stream.empty[Int].forAll(_ < 3))
  assert(Stream(1).forAll(_ < 3))
  assert(Stream(1, 2).forAll(_ < 3))
  assert(!Stream(1, 2, 3).forAll(_ < 3))

  var x = 0
  assert(!Stream(1, 2, 3, 4, 5).forAll(i => {
    x = i
    i < 3
  }))
  assert(x == 3)

  assert(Stream.empty[Int].takeWhileViaFoldRight(_ < 3).toList == Nil)
  assert(Stream(1).takeWhileViaFoldRight(_ > 3).toList == Nil)
  assert(Stream(1).takeWhileViaFoldRight(_ < 3).toList == List(1))
  assert(Stream(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList == List(1, 2))

}
