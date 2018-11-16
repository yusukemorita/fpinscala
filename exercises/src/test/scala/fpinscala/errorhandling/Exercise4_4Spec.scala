package fpinscala.errorhandling

import org.scalatest._

class Exercise4_4Spec extends FlatSpec {

  val sequenceByPatternMatch: List[Option[Int]] => Option[List[Int]] = Option.sequence
  val sequenceByFoldRight: List[Option[Int]] => Option[List[Int]] = Option.sequence2

  val functions = Map(
    "sequenceByPatternMatch" -> sequenceByPatternMatch,
    "sequenceByFoldRight" -> sequenceByFoldRight
  )

  functions.foreach{ case (key, function) =>
    s"$key(List(Some(1), Some(2)))" should "return Some(List(1, 2))" in {
      assert(function(List(Some(1), Some(2))) === Some(List(1, 2)))
    }

    s"$key(List(Some(1), Some(2), Some(3)))" should "return Some(List(1, 2, 3))" in {
      assert(function(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3)))
    }

    s"$key(List(None, Some(2), Some(3)))" should "return None" in {
      assert(function(List(None, Some(2), Some(3))) === None)
    }

    s"$key(List(Some(1), None, Some(3)))" should "return None" in {
      assert(function(List(Some(1), None,  Some(3))) === None)
    }

    s"$key(List(Some(1), Some(2), None))" should "return None" in {
      assert(function(List(Some(1), Some(2), None)) === None)
    }
  }
}
