package fpinscala.parallelism

import java.util.concurrent.Executors

import org.scalatest.FlatSpec

class Exercise7_5Spec extends FlatSpec {

  "_ * 2" should "work" in {
    val f1: Int => Int = _ * 2
    val f2: Int => Int = _ + 2

    val par1 = Par.asyncF(f1)(1)
    val par2 = Par.asyncF(f2)(1)

    val parList = List(par1, par2)


    val es = Executors.newFixedThreadPool(5)
    val result = Par.sequence(parList)(es).get()
    assert(result === List(2, 3))
  }
}
