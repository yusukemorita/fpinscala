package fpinscala.laziness

import org.scalatest._

class Exercise5_13Spec extends FlatSpec {

  // mapByUnfold

  "stream(1, 2, 3).mapByUnfold(_ * 2)" should "return stream(2, 4, 6)" in {
    val stream1 = Stream.cons(3, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream3 = Stream.cons(1, stream2)
    assert(stream3.mapByUnfold(_ * 2).toList === List(2, 4, 6))
  }

  "stream(1).mapByUnfold(_ + 1)" should "return stream(2)" in {
    val stream = Stream.cons(1, Empty)
    assert(stream.mapByUnfold(_ + 1).toList === List(2))
  }

  "empty.mapByUnfold(_ + 1)" should "return empty" in {
    val stream = Empty: Stream[Int]
    assert(stream.mapByUnfold(_ + 1) === Empty)
  }

  // takeViaUnfold

  "stream(1).takeViaUnfold(1)" should "return stream(1))" in {
    val stream = Stream.cons(1, Empty)
    assert(stream.takeViaUnfold(1).toList === stream.toList)
  }

  "stream(2, 1).takeViaUnfold(1)" should "return stream(2))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    assert(stream2.takeViaUnfold(1).toList === List(2))
  }

  "stream(2, 1).takeViaUnfold(3)" should "return stream(2, 1))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    assert(stream2.takeViaUnfold(3).toList === List(2, 1))
  }

  "stream(3, 2, 1).takeViaUnfold(3)" should "return stream(3, 2, 1))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream3 = Stream.cons(3, stream2)

    assert(stream3.takeViaUnfold(3).toList === List(3, 2, 1))
  }

  "empty.takeViaUnfold(3)" should "return empty" in {
    assert(Stream.empty.takeViaUnfold(3) === Empty)
  }

  // takeWhileViaUnfoldViaUnfold

  "stream(1).takeWhileViaUnfold(_ > 0)" should "return stream(1))" in {
    val stream = Stream.cons(1, Empty)
    assert(stream.takeWhileViaUnfold(_ > 0).toList === List(1))
  }

  "stream(2, 1).takeWhileViaUnfold(_ > 1)" should "return stream(2))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream = Stream.cons(2, stream1)
    assert(stream.takeWhileViaUnfold(_ > 1).toList === List(2))
  }

  "stream(3, 2, 1).takeWhileViaUnfold(_ % 2 == 1)" should "return stream(3))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream3 = Stream.cons(3, stream2)
    assert(stream3.takeWhileViaUnfold(_ % 2 == 1).toList === List(3))
  }

  // zipWith

  "zipWith(Stream(1, 2, 3), Stream(1, 2, 3)(_ * _)" should "return Stream(1, 4, 9)" in {
    val stream1 = Stream.cons(3, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream = Stream.cons(1, stream2)
    assert(stream.zipWith(stream)((a: Int, b: Int) => a * b).toList === List(1, 4, 9))
  }

  "zipWith(stream.zipWith(List(1, 2, 3), List(a, b, c)" should "return List(1a, 2b, 3c)" in {
    val stream1 = Stream.cons(3, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream = Stream.cons(1, stream2)

    val streamA = Stream.cons("c", Empty)
    val streamB = Stream.cons("b", streamA)
    val streamC = Stream.cons("a", streamB)
    assert(stream.zipWith(streamC)((a: Int, b: String) => a + b).toList === List("1a", "2b", "3c"))
  }

  // zipAll

  "zipAll(Stream(1, 2, 3), Stream(2, 3)" should "return Stream(Some(1), Some(2)), (Some(2), Some(3)), (Some(3), None))" in {
    val stream1 = Stream.cons(3, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream = Stream.cons(1, stream2)
    assert(stream.zipAll(stream2).toList === List((Some(1), Some(2)), (Some(2), Some(3)), (Some(3), None)))
  }
}
