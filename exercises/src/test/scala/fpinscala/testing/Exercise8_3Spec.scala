package fpinscala.testing

import fpinscala.state.RNG
import org.scalatest._

class Exercise8_3Spec extends FlatSpec {

  "inserting a coin when locked" should "unlock machine" in {
    val value = Gen.choose(5, 10)
    val sample = value.sample
    val rng = RNG.Simple(12345)

    val (i1, nextRng) = sample.run(rng)
    val (i2, _) = sample.run(nextRng)

    assert(i1 < 10 & i1 >= 5)
    assert(i2 < 10 & i2 >= 5)
  }
}
