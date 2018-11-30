package fpinscala.state

import fpinscala.state.RNG.Rand
import org.scalatest._

class Exercise6_10Spec extends FlatSpec {

  // unit

  "unit" should "work" in {
    val rng = RNG.int
    val state: State[Rand[Int], Int] = State.unit(1)

    val (int1, nextRng1) = state.run(rng)
    val (int2, nextRng2) = state.run(rng)

    assert(int1 === int2)
    assert(nextRng1 === nextRng2)
  }

  // map

  "map" should "work" in {
    val nonNegativeIntRand = RNG.int
    val state = new State(nonNegativeIntRand)
    val a: State[RNG, Int] = state.map(i => i * 2)
    val rng = RNG.Simple(12345)

    val (i, _) = a.run(rng)

    assert(i % 2 === 0)
  }

  // flatMap

  "flatMap" should "work" in {
    val randInt: Rand[Int] = RNG.int
    val randDouble = RNG.flatMap(randInt)(_ => RNG.doubleViaMap)
    val state = new State(randDouble)

    val rng = RNG.Simple(12345)

    val double1, nextRng1 = state.run(rng)
    val double2, nextRng2 = state.run(rng)

    assert(double1 == double2)
    assert(nextRng1 === nextRng2)
  }

  // map2

  "map2" should "work" in {
    val randInt: Rand[Int] = RNG.int
    val randDouble = RNG.flatMap(randInt)(_ => RNG.doubleViaMap)

    val state1: State[RNG, Int] = new State(randInt)
    val state2: State[RNG, Double] = new State(randDouble)

    val func = state1.map2(state2)((a: Int, b: Double) => a.toString + b.toString)

    val rng: RNG = RNG.Simple(12345)

    val (expectedInt, nextRng) = state1.run(rng)
    val (expectedDouble, _) = state2.run(nextRng)
    val expectedString = expectedInt.toString + expectedDouble.toString

    val (resultString, _) = func.run(rng)

    assert(expectedString === resultString)
  }
}
