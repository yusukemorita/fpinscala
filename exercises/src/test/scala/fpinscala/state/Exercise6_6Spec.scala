package fpinscala.state

import fpinscala.state.RNG.Rand
import org.scalatest._

class Exercise6_6Spec extends FlatSpec {

  "map2" should "work" in {
    val func: Rand[(Int, Int)] = RNG.map2(RNG.int, RNG.int)((_, _))
    val rng = RNG.Simple(12345)
    val ((a1, b1), nextRng1) = func(rng)
    val ((a2, b2), nextRng2) = func(rng)

    assert(a1 == a2)
    assert(b1 == b2)
    assert(nextRng1 === nextRng2)
  }
}
