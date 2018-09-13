package fpinscala.gettingstarted

import org.scalatest._

class Exercise2_1Spec extends FlatSpec {

  val fibonacciMap = Map(
    -1 -> 0, 1 -> 0,  2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3,  6 -> 5,  7 -> 8,  8 -> 13
  )

  for ((key, value) <- fibonacciMap) {
    s"fib($key)" should s"return $value" in {
      assert(MyModule.fib(key) == value)
    }
  }

}
