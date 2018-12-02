package fpinscala.parallelism

import java.util.concurrent.Executors

import org.scalatest.FlatSpec

class Exercise7_6Spec extends FlatSpec {

  val es = Executors.newFixedThreadPool(5)

  "_ * 2" should "work" in {
    val list = List(1, 2, 3, 4, 5, 6)
    val isEven: Int => Boolean = _ % 2 == 0
    val parList = Par.parFilter(list)(isEven)
    val filteredList = Par.run(es)(parList).get()

    assert(filteredList === list.filter(isEven))
  }
}
