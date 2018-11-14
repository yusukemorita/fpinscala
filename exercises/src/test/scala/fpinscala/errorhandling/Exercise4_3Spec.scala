package fpinscala.errorhandling

import org.scalatest._

class Exercise4_3Spec extends FlatSpec {

  "Option.map2(Some(1), Some(2))(_ + _)" should s"return Some(3)" in {
    assert(Option.map2(Some(1), Some(2))(_ + _) === Some(3))
  }

  "Option.map2(Some(1), None)(_ + _)" should s"return Some(3)" in {
    assert(Option.map2(Some(1), None)(_ + _) === None)
  }

  "Option.map2(None: Option[Int], Some(2))(_ + _)" should s"return Some(3)" in {
    assert(Option.map2(None: Option[Int], Some(2))(_ + _) === None)
  }
}
