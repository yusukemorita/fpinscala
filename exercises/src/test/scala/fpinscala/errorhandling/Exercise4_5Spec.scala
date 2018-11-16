package fpinscala.errorhandling

import org.scalatest._

class Exercise4_5Spec extends FlatSpec {

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }

  "Option.traverse(List('1', '2'))(toInt)" should "return " in {
    assert(Option.traverse(List("1", "2"))(toInt) === Some(List(1, 2)))
  }
}
