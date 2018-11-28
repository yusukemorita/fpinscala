package fpinscala.state

import fpinscala.state.RNG.Rand
import org.scalatest._

class Exercise6_8Spec extends FlatSpec {

  // flatMap

  "flatMap" should "work" in {
    val randInt: Rand[Int] = RNG.int
    val randDouble = RNG.flatMap(randInt)(_ => RNG.doubleViaMap)

    val rng = RNG.Simple(12345)

    val double1, nextRng1 = randDouble(rng)
    val double2, nextRng2 = randDouble(rng)

    assert(double1 == double2)
    assert(nextRng1 === nextRng2)
  }

  // nonNegativeLessThan

  "nonNegativeLessThan 1" should "return 0" in {
    val rng = RNG.Simple(123)
    val (i, nextRng) = RNG.nonNegativeLessThan(1)(rng)

    assert(i === 0)
  }

  // nonNegativeLessThan

  "nonNegativeLessThan 2" should "return 0 or 1" in {
    val rng = RNG.Simple(123)
    val (i, nextRng) = RNG.nonNegativeLessThan(2)(rng)

    assert(Seq(0, 1).contains(i))
  }
}
