package fpis.ch5

object Main extends App {

  var x = 0

  def resetX() = x = 0

  def sstream[A](a: A*)(se: => Unit): Stream[A] =
    if (a.isEmpty) Empty
    else Cons(() => {
      se
      a.head
    }, () => sstream(a.tail: _*)(se))

  def XStream[A](a: A*): Stream[A] = sstream(a: _*)(x += 1)

  assert(Stream.empty.toList == Nil)
  assert(Stream(1).toList == List(1))
  resetX()
  assert(XStream(1, 2, 3).toList == List(1, 2, 3))
  assert(x == 3)

  assert(Stream.empty.take(0).toList == Nil)
  assert(Stream.empty.take(10).toList == Nil)
  assert(Stream(1).take(0).toList == Nil)
  assert(Stream(1).take(1).toList == List(1))
  assert(Stream(1).take(10).toList == List(1))

  resetX()
  val stream = XStream(1, 2, 3, 4, 5)
  assert(x == 0)
  assert(stream.take(2).toList == List(1, 2))
  assert(x == 2)

  assert(Stream.empty.drop(0).toList == Nil)
  assert(Stream.empty.drop(10).toList == Nil)
  assert(Stream(1).drop(0).toList == List(1))
  assert(Stream(1).drop(1).toList == Nil)
  assert(Stream(1).drop(10).toList == Nil)

  resetX()
  val streamD = XStream(1, 2, 3, 4, 5)
  assert(x == 0)
  assert(streamD.drop(2).toList == List(3, 4, 5))
  assert(x == 3)

  assert(Stream.empty[Int].takeWhile(_ < 3).toList == Nil)
  assert(Stream(1).takeWhile(_ > 3).toList == Nil)
  assert(Stream(1).takeWhile(_ < 3).toList == List(1))
  resetX()
  assert(XStream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList == List(1, 2))
  assert(x == 3)

  resetX()
  val tw = XStream(1, 2, 3, 4, 5).takeWhile(_ < 4)
  assert(x == 1)
  val twt = tw.take(2)
  assert(x == 1)
  assert(twt.toList == List(1, 2))
  assert(x == 2)

  assert(Stream.empty[Int].forAll(_ < 3))
  assert(Stream(1).forAll(_ < 3))
  assert(Stream(1, 2).forAll(_ < 3))
  assert(!Stream(1, 2, 3).forAll(_ < 3))

  resetX()
  assert(!XStream(1, 2, 3, 4, 5).forAll(_ < 3))
  assert(x == 3)

  assert(Stream.empty[Int].takeWhileViaFoldRight(_ < 3).toList == Nil)
  assert(Stream(1).takeWhileViaFoldRight(_ > 3).toList == Nil)
  assert(Stream(1).takeWhileViaFoldRight(_ < 3).toList == List(1))
  resetX()
  assert(XStream(1, 2, 3, 4, 5, 6, 7).takeWhileViaFoldRight(_ < 3).toList == List(1, 2))
  assert(x == 3)

  assert(Stream.empty[Int].headOption == None)
  assert(Stream(1).headOption == Some(1))
  assert(Stream(1, 2, 3).headOption == Some(1))
  resetX()
  assert(XStream(1, 2, 3).headOption == Some(1))
  assert(x == 1)

  assert(Stream.empty[Int].map(_ + 1).toList == Nil)
  assert(Stream(1).map(_ + 1).toList == List(2))

  resetX()
  val mapped = XStream(1, 2, 3, 4).map(_ + 1)
  assert(x == 1)
  val twoMapped = mapped.take(2)
  assert(x == 1)
  assert(twoMapped.toList == List(2, 3))
  assert(x == 2)

  assert(Stream.empty[Int].filter(_ < 3).toList == Nil)
  assert(Stream(1).filter(_ < 3).toList == List(1))
  resetX()
  assert(XStream(1, 2, 3, 4, 5).filter(_ < 3).toList == List(1, 2))
  assert(x == 5)

  resetX()
  val filtered = XStream(1, 2, 3, 4, 5).filter(_ < 3)
  assert(x == 1)
  val twoFiltered = filtered.take(2)
  assert(x == 1)
  assert(twoFiltered.toList == List(1, 2))
  assert(x == 2)

  assert(Stream.empty.append(Stream.empty).toList == Nil)
  assert(Stream(1).append(Stream.empty).toList == List(1))
  assert(Stream.empty.append(Stream(1)).toList == List(1))
  assert(Stream(1).append(Stream(2)).toList == List(1, 2))
  resetX()
  val app = XStream(1, 2, 3).append(XStream(4, 5, 6))
  assert(x == 1)
  assert(app.toList == List(1, 2, 3, 4, 5, 6))
  assert(x == 6)

  val f = (i: Int) => Stream(i + 1)

  assert(Stream.empty[Int].flatMap(f).toList == Nil)
  assert(Stream(1).flatMap(f).toList == List(2))

  resetX()
  val flatMapped = XStream(1, 2, 3, 4).flatMap(f)

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

  assert(Stream.empty[Int].mapViaUnfold(_ + 1).toList == Nil)
  assert(Stream(1).mapViaUnfold(_ + 1).toList == List(2))

  resetX()
  val mappedUnfold = XStream(1, 2, 3, 4).mapViaUnfold(_ + 1)
  assert(x == 1)
  val twoMappedUnfold = mappedUnfold.take(2)
  assert(x == 1)
  assert(twoMappedUnfold.toList == List(2, 3))
  assert(x == 2)

  assert(Stream.empty.takeViaUnfold(0).toList == Nil)
  assert(Stream.empty.takeViaUnfold(10).toList == Nil)
  assert(Stream(1).takeViaUnfold(0).toList == Nil)
  assert(Stream(1).takeViaUnfold(1).toList == List(1))
  assert(Stream(1).takeViaUnfold(10).toList == List(1))

  resetX()
  val stream2 = XStream(1, 2, 3)
  assert(x == 0)
  assert(stream2.takeViaUnfold(2).toList == List(1, 2))
  assert(x == 2)

  assert(Stream.empty[Int].takeWhileViaUnfold(_ < 3).toList == Nil)
  assert(Stream(1).takeWhileViaUnfold(_ > 3).toList == Nil)
  assert(Stream(1).takeWhileViaUnfold(_ < 3).toList == List(1))

  resetX()
  val stream3 = XStream(1, 2, 3, 4, 5)
  assert(x == 0)
  assert(stream3.takeWhileViaUnfold(_ < 3).toList == List(1, 2))
  assert(x == 3)


  resetX()
  val tw2 = XStream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ < 4)
  assert(x == 1)
  val twt2 = tw2.takeViaUnfold(2)
  assert(x == 2) // why not x == 1?
  assert(twt2.toList == List(1, 2))
  assert(x == 2)

  resetX()
  val tw3 = XStream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ < 4)
  assert(x == 1)
  val twt3 = tw3.takeViaUnfold(3)
  assert(x == 2) // why not x == 1?
  assert(twt3.toList == List(1, 2, 3))
  assert(x == 3)

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

  assert(Stream.startsWith(Stream.empty, Stream.empty))
  assert(Stream.startsWith(Stream(1), Stream(1)))
  assert(Stream.startsWith(Stream(1, 2, 3), Stream(1, 2, 3)))
  assert(Stream.startsWith(Stream(1, 2, 3), Stream(1, 2)))
  assert(Stream.startsWith(Stream(1, 2, 3), Stream(1)))
  assert(Stream.startsWith(Stream(1, 2, 3), Stream.empty))

  assert(!Stream.startsWith(Stream(1, 2, 3), Stream(1, 2, 4)))
  assert(!Stream.startsWith(Stream(1, 2, 3), Stream(1, 2, 4)))
  assert(!Stream.startsWith(Stream(1, 2, 3), Stream(1, 2, 3, 4)))

  assert(Empty.tails.toList == List(Empty))
  assert(Stream(1).tails.toList.size == 2)
  assert(Stream(1).tails.toList.flatMap(_.toList) == List(1))

  assert(Stream(1, 2).tails.toList.size == 3)
  assert(Stream(1, 2).tails.toList.flatMap(_.toList) == List(1, 2, 2))

  assert(Stream(1, 2, 3).tails.toList.size == 4)
  assert(Stream(1, 2, 3).tails.toList.flatMap(_.toList) == List(1, 2, 3, 2, 3, 3))

  assert(Empty.tailsViaScanRight.toList == List(Empty))
  assert(Stream(1).tailsViaScanRight.toList.size == 2)
  assert(Stream(1).tailsViaScanRight.toList.flatMap(_.toList) == List(1))

  assert(Stream(1, 2).tailsViaScanRight.toList.size == 3)
  assert(Stream(1, 2).tailsViaScanRight.toList.flatMap(_.toList) == List(1, 2, 2))

  assert(Stream(1, 2, 3).tailsViaScanRight.toList.size == 4)
  assert(Stream(1, 2, 3).tailsViaScanRight.toList.flatMap(_.toList) == List(1, 2, 3, 2, 3, 3))
}