package fpinscala.datastructures

import org.scalatest._
import fpinscala.ExecutionTimeMeasurer.measureTime

class Exercise3_4Spec extends FlatSpec {

  val passingTestCases = Seq(
    (List(1, 2, 3, 4), 2, List(3, 4)),
    (List(1, 2, 3, 4), 3, List(4)),
    (List(1, 2, 3, 4), 4, Nil),
    (Nil, 0, Nil),
    (List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 1, List(2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0)),
    (List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 10, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)),
    (List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 20, Nil)
  )

  for ((list, number, output) <- passingTestCases) {
    s"drop $number of $list" should s"return $output" in {
      val resultList = measureTime{ List.drop(list,  number)}
      assert(resultList === output)
    }
  }

  val exceptionThrowingTestCases = Seq(
    (Nil, 1),
    (List(1), 2)
  )

  for ((list, number) <- exceptionThrowingTestCases) {
    s"drop $number of $list" should s"throw exception" in {
      assertThrows[Exception](List.drop(list,  number))
    }
  }
}
