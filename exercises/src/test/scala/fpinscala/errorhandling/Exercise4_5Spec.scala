package fpinscala.errorhandling

import org.scalatest._

class Exercise4_5Spec extends FlatSpec {

  // traverse

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }

  "Option.traverse(List('1', '2'))(toInt)" should "return Some(List(1, 2))" in {
    assert(Option.traverse(List("1", "2"))(toInt) === Some(List(1, 2)))
  }

  "Option.traverse(List('1', 'a'))(toInt)" should "return None" in {
    assert(Option.traverse(List("1", "a"))(toInt) === None)
  }

  // traverse2

  "Option.traverse2(List('1', '2'))(toInt)" should "return Some(List(1, 2))" in {
    assert(Option.traverse2(List("1", "2"))(toInt) === Some(List(1, 2)))
  }

  "Option.traverse2(List('1', 'a'))(toInt)" should "return None" in {
    assert(Option.traverse2(List("1", "a"))(toInt) === None)
  }

  // sequence3

  "Option.sequence3(List(Some(1), Some(2)))" should "return Some(List(1, 2))" in {
    assert(Option.sequence3(List(Some(1), Some(2))) === Some(List(1, 2)))
  }

  "Option.sequence3(List(Some(1), Some(2), Some(3)))" should "return Some(List(1, 2, 3))" in {
    assert(Option.sequence3(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3)))
  }

  "Option.sequence3(List(None, Some(2), Some(3)))" should "return None" in {
    assert(Option.sequence3(List(None, Some(2), Some(3))) === None)
  }

  "Option.sequence3(List(Some(1), None, Some(3)))" should "return None" in {
    assert(Option.sequence3(List(Some(1), None,  Some(3))) === None)
  }

  "Option.sequence3(List(Some(1), Some(2), None))" should "return None" in {
    assert(Option.sequence3(List(Some(1), Some(2), None)) === None)
  }
}
