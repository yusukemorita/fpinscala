package fpinscala.errorhandling

import org.scalatest._

class Exercise4_2Spec extends FlatSpec {

  "mean(Seq(0.0, 2.0)" should s"return Some(1.0)" in {
    assert(Option.variance(Seq(0.0, 2.0)) === Some(1.0))
  }

  "mean(Nil)" should s"return None" in {
    assert(Option.variance(Nil) === None)
  }

}
