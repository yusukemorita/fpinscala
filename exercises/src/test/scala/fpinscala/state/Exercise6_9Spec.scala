package fpinscala.state

import fpinscala.state.RNG.Rand
import org.scalatest._

class Exercise6_9Spec extends FlatSpec {

  // mapViaFlatMap

  "map" should "work" in {
    val nonNegativeIntRand = RNG.mapViaFlatMap(RNG.nonNegativeInt)(i => i % 2)
    val rng = RNG.Simple(12345)

    val (i, _) = nonNegativeIntRand(rng)

    assert(Seq(0, 1).contains(i))
  }

  // map2ViaFlatMap

  "map2" should "work" in {
    val func: Rand[(Int, Int)] = RNG.map2ViaFlatMap(RNG.int, RNG.int)((_, _))
    val rng = RNG.Simple(12345)
    val ((a1, b1), nextRng1) = func(rng)
    val ((a2, b2), nextRng2) = func(rng)

    assert(a1 == a2)
    assert(b1 == b2)
    assert(nextRng1 === nextRng2)
  }

}
