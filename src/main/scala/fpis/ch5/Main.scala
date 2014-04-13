package fpis.ch5

import Stream.{cons => Cons}

object Main extends App {

  assert(Stream.empty.toList == Nil)
  assert(Stream(1).toList == List(1))
  assert(Stream(1, 2, 3).toList == List(1, 2, 3))

  assert(Stream.empty.take(0).toList == Nil)
  assert(Stream.empty.take(10).toList == Nil)
  assert(Stream(1).take(0).toList == Nil)
  assert(Stream(1).take(1).toList == List(1))
  assert(Stream(1).take(10).toList == List(1))

  var x = 0

  def ix(i: Int) = {
    x += 1
    i
  }

  val stream = Cons(ix(1), Cons(ix(2), Cons(ix(3), Empty)))
  assert(x == 0)
  assert(stream.take(2).toList == List(1, 2))
  assert(x == 2)

  assert(Stream.empty[Int].takeWhile(_ < 3).toList == Nil)
  assert(Stream(1).takeWhile(_ > 3).toList == Nil)
  assert(Stream(1).takeWhile(_ < 3).toList == List(1))
  assert(Stream(1, 2, 3).takeWhile(_ < 3).toList == List(1, 2))

  assert(Stream.empty[Int].forAll(_ < 3))
  assert(Stream(1).forAll(_ < 3))
  assert(Stream(1, 2).forAll(_ < 3))
  assert(!Stream(1, 2, 3).forAll(_ < 3))

  x = 0
  assert(!Stream(1, 2, 3, 4, 5).forAll(i => {
    x = i
    i < 3
  }))
  assert(x == 3)

  assert(Stream.empty[Int].takeWhileViaFoldRight(_ < 3).toList == Nil)
  assert(Stream(1).takeWhileViaFoldRight(_ > 3).toList == Nil)
  assert(Stream(1).takeWhileViaFoldRight(_ < 3).toList == List(1))
  assert(Stream(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList == List(1, 2))


  assert(Stream.empty[Int].map(_ + 1).toList == Nil)
  assert(Stream(1).map(_ + 1).toList == List(2))

  x = 0
  val mapped = Stream(1, 2, 3, 4).map(i => {
    x += 1
    i + 1
  })
  assert(x == 0)
  val twoMapped = mapped.take(2)
  assert(x == 1)
  assert(twoMapped.toList == List(2, 3))
  assert(x == 2)

  assert(Stream.empty[Int].filter(_ < 3).toList == Nil)
  assert(Stream(1).filter(_ < 3).toList == List(1))
  assert(Stream(1, 2, 3).filter(_ < 3).toList == List(1, 2))

  x = 0
  val filtered = Stream(1, 2, 3, 4).filter(i => {
    x += 1
    i < 3
  })
  assert(x == 1)
  val twoFiltered = filtered.take(2)
  assert(x == 1)
  assert(twoFiltered.toList == List(1, 2))
  assert(x == 2)


  assert(Stream.empty.append(Stream.empty).toList == Nil)
  assert(Stream(1).append(Stream.empty).toList == List(1))
  assert(Stream.empty.append(Stream(1)).toList == List(1))
  assert(Stream(1).append(Stream(2)).toList == List(1, 2))
  assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))


  val f = (i: Int) => {
    x += 1
    Stream(i + 1)
  }
  assert(Stream.empty[Int].flatMap(f).toList == Nil)
  assert(Stream(1).flatMap(f).toList == List(2))

  x = 0
  val flatMapped = Stream(1, 2, 3, 4).flatMap(f)

  assert(x == 1)
  val twoFlatMapped = flatMapped.take(2)
  assert(x == 1)
  assert(twoFlatMapped.toList == List(2, 3))
  assert(x == 2)

  assert(Stream.constant(1).take(0).toList == Nil)
  assert(Stream.constant(1).take(1).toList == List(1))
  assert(Stream.constant(1).take(3).toList == List(1, 1, 1))

  assert(Stream.constant2(1).take(0).toList == Nil)
  assert(Stream.constant2(1).take(1).toList == List(1))
  assert(Stream.constant2(1).take(3).toList == List(1, 1, 1))

  assert(Stream.from(1).take(0).toList == Nil)
  assert(Stream.from(1).take(1).toList == List(1))
  assert(Stream.from(1).take(3).toList == List(1, 2, 3))

  assert(Stream.fibs.take(0).toList == Nil)
  assert(Stream.fibs.take(1).toList == List(0))
  assert(Stream.fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))


  assert(Stream.constantViaUnfold(1).take(0).toList == Nil)
  assert(Stream.constantViaUnfold(1).take(1).toList == List(1))
  assert(Stream.constantViaUnfold(1).take(3).toList == List(1, 1, 1))

  assert(Stream.onesViaUnfold.take(0).toList == Nil)
  assert(Stream.onesViaUnfold.take(1).toList == List(1))
  assert(Stream.onesViaUnfold.take(3).toList == List(1, 1, 1))

  assert(Stream.fromViaUnfold(1).take(0).toList == Nil)
  assert(Stream.fromViaUnfold(1).take(1).toList == List(1))
  assert(Stream.fromViaUnfold(1).take(3).toList == List(1, 2, 3))

  assert(Stream.fibsViaUnfold.take(0).toList == Nil)
  assert(Stream.fibsViaUnfold.take(1).toList == List(0))
  assert(Stream.fibsViaUnfold.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))

  assert(Stream.empty[Int].map(_ + 1).toList == Nil)
  assert(Stream(1).map(_ + 1).toList == List(2))

  x = 0
  val mappedUnfold = Stream(1, 2, 3, 4).mapViaUnfold(i => {
    x += 1
    i + 1
  })
  assert(x == 1) // here mapViaUnfold will intitialize head
  val twoMappedUnfold = mappedUnfold.take(2)
  assert(x == 1)
  assert(twoMappedUnfold.toList == List(2, 3))
  assert(x == 2)

  assert(Stream.empty.takeViaUnfold(0).toList == Nil)
  assert(Stream.empty.takeViaUnfold(10).toList == Nil)
  assert(Stream(1).takeViaUnfold(0).toList == Nil)
  assert(Stream(1).takeViaUnfold(1).toList == List(1))
  assert(Stream(1).takeViaUnfold(10).toList == List(1))

  x = 0
  val stream2 = Cons(ix(1), Cons(ix(2), Cons(ix(3), Cons(ix(4), Empty))))
  assert(x == 0)
  assert(stream2.takeViaUnfold(2).toList == List(1, 2))
  assert(x == 2)

  assert(Stream.empty[Int].takeWhileViaUnfold(_ < 3).toList == Nil)
  assert(Stream(1).takeWhileViaUnfold(_ > 3).toList == Nil)
  assert(Stream(1).takeWhileViaUnfold(_ < 3).toList == List(1))
  assert(Stream(1, 2, 3).takeWhileViaUnfold(_ < 3).toList == List(1, 2))

  assert(Stream.empty[Int].zip(Stream.empty[Int]).toList == Nil)
  assert(Stream.empty[Int].zip(Stream(1, 2, 3)).toList == Nil)
  assert(Stream(1, 2, 3).zip(Stream.empty[Int]).toList == Nil)
  assert(Stream(1, 2, 3).zip(Stream(4, 5, 6)).toList == List(1 -> 4, 2 -> 5, 3 -> 6))
  assert(Stream(1, 2, 3).zip(Stream(4, 5)).toList == List(1 -> 4, 2 -> 5))
  assert(Stream(1, 2).zip(Stream(4, 5, 6)).toList == List(1 -> 4, 2 -> 5))

  assert(Stream.empty[Int].zipAll(Stream.empty[Int]).take(3).toList == Nil)
  assert(Stream.empty[Int].zipAll(Stream(1, 2, 3)).take(3).toList == List(None -> Some(1), None -> Some(2), None -> Some(3)))
  assert(Stream(1, 2, 3).zipAll(Stream.empty[Int]).take(3).toList == List(Some(1) -> None, Some(2) -> None, Some(3) -> None))
  assert(Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).take(3).toList == List(Some(1) -> Some(4), Some(2) -> Some(5), Some(3) -> Some(6)))

  assert(Stream.empty[Int].zipAll(Stream.empty[Int]).toList == Nil)
  assert(Stream.empty[Int].zipAll(Stream(1, 2, 3)).toList == List(None -> Some(1), None -> Some(2), None -> Some(3)))
  assert(Stream(1, 2, 3).zipAll(Stream.empty[Int]).toList == List(Some(1) -> None, Some(2) -> None, Some(3) -> None))
  assert(Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).toList == List(Some(1) -> Some(4), Some(2) -> Some(5), Some(3) -> Some(6)))

}