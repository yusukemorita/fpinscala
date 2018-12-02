package fpinscala.parallelism

import java.util.concurrent.Executors

import org.scalatest.FlatSpec

class Exercise7_4Spec extends FlatSpec {

  "_ * 2" should "work" in {
    val f: Int => Int = _ * 2
    val es = Executors.newFixedThreadPool(5)

    val par = Par.asyncF(f)
    val result = Par.run(es)(par(2)).get()
    assert(result === 2 * 2)
  }
}
