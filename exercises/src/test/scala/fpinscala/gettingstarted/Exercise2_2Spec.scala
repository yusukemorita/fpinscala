package fpinscala.gettingstarted

import org.scalatest._

class Exercise2_2Spec extends FlatSpec {

  val isSmaller: (Int, Int) => Boolean = (x, y) => x < y

  val numbersAndResults = Seq(
    (Array(1, 2), true),
    (Array(1, 2, 3), true),
    (Array(2, 3, 5, 8), true),
    (Array(2, 1), false),
    (Array(3, 2, 1), false),
    (Array(3, 8, 7), false)
  )

  for ((numbers, result) <- numbersAndResults) {
    s"(${numbers.mkString(",")})" should s"return $result" in {
      assert(PolymorphicFunctions.isSorted(numbers, isSmaller) == result )
    }
  }

  val isShorter: (String, String) => Boolean = (x, y) => x.length() < y.length()

  val stringsAndResults = Seq(
    (Array("hi", "hello"), true),
    (Array("aa", "abc", "hippopotamus"), true),
    (Array("abc", "aa", "hippopotamus"), false)
  )

  for ((array, result) <- stringsAndResults) {
    s"(${array.mkString(",")})" should s"return $result" in {
      assert(PolymorphicFunctions.isSorted(array, isShorter) == result)
    }
  }
}
