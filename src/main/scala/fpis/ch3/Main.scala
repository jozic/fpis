package fpis.ch3

object Main extends App {
  assert(ex1.x == 3)

  assert(List.tail(List(1, 2, 3)) == List(2, 3))

  assert(List.setHead(Nil, 5) == List(5))
  assert(List.setHead(List(1, 2, 3), 5) == List(5, 2, 3))

  assert(List.drop(Nil, 0) == Nil)
  assert(List.drop(List(1, 2, 3), 0) == List(1, 2, 3))
  assert(List.drop(List(1, 2, 3), 1) == List(2, 3))
  assert(List.drop(List(1, 2, 3), 2) == List(3))
  assert(List.drop(List(1, 2, 3), 3) == Nil)
  assert(List.drop(List(1, 2, 3), 4) == Nil)


  assert(List.dropWhile(List(1, 2, 3))(_ < 4) == Nil)
  assert(List.dropWhile(List(1, 2, 3))(_ < 3) == List(3))
  assert(List.dropWhile(List(1, 2, 3))(_ < 2) == List(2, 3))
  assert(List.dropWhile(List(1, 2, 3))(_ < 1) == List(1, 2, 3))
  assert(List.dropWhile(List(1, 2, 3))(_ < 0) == List(1, 2, 3))
  assert(List.dropWhile[Int](Nil)(_ < 0) == Nil)


  assert(List.init(List(1, 2, 3)) == List(1, 2))
  assert(List.init(List(1, 2)) == List(1))
  assert(List.init(List(1)) == Nil)


  // ex 8
  assert(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons.apply) == List(1, 2, 3))

  assert(List.length(List(1, 2, 3)) == 3)
  assert(List.length(List(1, 2)) == 2)
  assert(List.length(List(1)) == 1)
  assert(List.length(Nil) == 0)

  assert(List.sum3(List(1, 2, 3, 4)) == 10.0)
  assert(List.product3(List(1, 2, 3, 4)) == 24.0)

  assert(List.length3(List(1, 2, 3)) == 3)
  assert(List.length3(List(1, 2)) == 2)
  assert(List.length3(List(1)) == 1)
  assert(List.length3(Nil) == 0)

  assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  assert(List.reverse(List(1, 2)) == List(2, 1))
  assert(List.reverse(List(1)) == List(1))
  assert(List.reverse(Nil) == Nil)

  assert(List.foldLeftInFoldRight(List(1, 2, 3), Nil: List[Int])((r, a) => Cons(a, r)) == List(3, 2, 1))

  assert(List.append2(Nil, Nil) == Nil)
  assert(List.append2(Nil, List(1)) == List(1))
  assert(List.append2(List(1), Nil) == List(1))

  assert(List.append2(List(1), List(2)) == List(1, 2))


  assert(List.concat(Nil) == Nil)
  assert(List.concat(List(Nil)) == Nil)
  assert(List.concat(List(Nil, Nil)) == Nil)
  assert(List.concat(List(Nil, List(Nil))) == List(Nil))
  assert(List.concat(List(Nil, List(1))) == List(1))
  assert(List.concat(List(Nil, List(1), List(2, 3), Nil)) == List(1, 2, 3))

  assert(List.plus1(Nil) == Nil)
  assert(List.plus1(List(1)) == List(2))
  assert(List.plus1(List(1, 2, 3)) == List(2, 3, 4))

  assert(List.doublesToStrings(Nil) == Nil)
  assert(List.doublesToStrings(List(1.0)) == List("1.0"))

  assert(List.plus1_2(Nil) == Nil)
  assert(List.plus1_2(List(1)) == List(2))
  assert(List.plus1_2(List(1, 2, 3)) == List(2, 3, 4))

  assert(List.doublesToStrings_2(Nil) == Nil)
  assert(List.doublesToStrings_2(List(1.0)) == List("1.0"))


  assert(List.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4))

  assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))

  assert(List.filterInFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4))

  assert(List.zip(Nil, Nil) == Nil)
  assert(List.zip(List(4, 5, 6), Nil) == Nil)
  assert(List.zip(Nil, List(4, 5, 6)) == Nil)
  assert(List.zip(List(1, 2, 3), List(4, 5, 6)) == List((1, 4), (2, 5), (3, 6)))
  assert(List.zip(List(1, 2, 3), List(4, 5, 6, 7, 8, 9)) == List((1, 4), (2, 5), (3, 6)))

  assert(List.zipAdd(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))

  assert(List.zipMap(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))

  assert(List.zipMap2(List(1, 2, 3, 4), List(4, 5, 6))(_ + _) == List(5, 7, 9))

  assert(List.hasSubsequence(Nil, Nil))

  assert(List.hasSubsequence(List(1), Nil))
  assert(List.hasSubsequence(List(1, 2, 3), Nil))

  assert(List.hasSubsequence(List(1, 2, 3), List(1)))
  assert(List.hasSubsequence(List(1, 2, 3), List(2)))
  assert(List.hasSubsequence(List(1, 2, 3), List(3)))
  assert(List.hasSubsequence(List(1, 2, 3), List(1, 2)))
  assert(List.hasSubsequence(List(1, 2, 3), List(2, 3)))
  assert(List.hasSubsequence(List(1, 2, 3), List(1, 2, 3)))

  assert(!List.hasSubsequence(List(1, 2, 3), List(1, 2, 3, 4)))
  assert(!List.hasSubsequence(List(1, 2, 3), List(1, 3)))
  assert(!List.hasSubsequence(Nil, List(1, 3)))



  assert(Tree.size(Leaf(1)) == 1)
  assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
  assert(Tree.size(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) == 5)

  assert(Tree.maximum(Leaf(1)) == 1)
  assert(Tree.maximum(Leaf(-5)) == -5)
  assert(Tree.maximum(Branch(Leaf(1), Leaf(2))) == 2)
  assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(-4)), Branch(Leaf(2), Leaf(3)))) == 3)

  assert(Tree.depth(Leaf(1)) == 1)
  assert(Tree.depth(Branch(Leaf(1), Leaf(2))) == 2)
  assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(-4)), Branch(Leaf(2), Leaf(3)))) == 3)
  assert(Tree.depth(Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(4))), Branch(Leaf(2), Leaf(3)))) == 4)

  val f = (i: Int) => i + 1
  assert(Tree.map(Leaf(1))(f) == Leaf(2))
  assert(Tree.map(Branch(Leaf(1), Leaf(2)))(f) == Branch(Leaf(2), Leaf(3)))
  assert(Tree.map(Branch(Branch(Leaf(1), Leaf(-4)), Branch(Leaf(2), Leaf(3))))(f) == Branch(Branch(Leaf(2), Leaf(-3)), Branch(Leaf(3), Leaf(4))))

  assert(Tree.sizeFold(Leaf(1)) == 1)
  assert(Tree.sizeFold(Branch(Leaf(1), Leaf(2))) == 3)
  assert(Tree.sizeFold(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) == 5)

  assert(Tree.maximumFold(Leaf(1)) == 1)
  assert(Tree.maximumFold(Leaf(-5)) == -5)
  assert(Tree.maximumFold(Branch(Leaf(1), Leaf(2))) == 2)
  assert(Tree.maximumFold(Branch(Branch(Leaf(1), Leaf(-4)), Branch(Leaf(2), Leaf(3)))) == 3)

  assert(Tree.depthFold(Leaf(1)) == 1)
  assert(Tree.depthFold(Branch(Leaf(1), Leaf(2))) == 2)
  assert(Tree.depthFold(Branch(Branch(Leaf(1), Leaf(-4)), Branch(Leaf(2), Leaf(3)))) == 3)
  assert(Tree.depthFold(Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(4))), Branch(Leaf(2), Leaf(3)))) == 4)

  assert(Tree.mapFold(Leaf(1))(f) == Leaf(2))
  assert(Tree.mapFold(Branch(Leaf(1), Leaf(2)))(f) == Branch(Leaf(2), Leaf(3)))
  assert(Tree.mapFold(Branch(Branch(Leaf(1), Leaf(-4)), Branch(Leaf(2), Leaf(3))))(f) == Branch(Branch(Leaf(2), Leaf(-3)), Branch(Leaf(3), Leaf(4))))

}