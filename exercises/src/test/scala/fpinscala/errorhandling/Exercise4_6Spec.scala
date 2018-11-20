package fpinscala.errorhandling

import org.scalatest._

class Exercise4_6Spec extends FlatSpec {

  // map

  "right.map(_ + 1)" should "return Right(2)" in {
    val right = Right(1)
    assert(right.map(_ + 1) === Right(2))
  }

  "Left('error')" should "return Left('error')" in {
    val left: Either[String, Int] = Left("error")
    assert(left.map(_ + 1) === Left("error"))
  }

  // flatMap

  "right.flatMap" should "return Right(2)" in {
    val right = Right(1)
    assert(right.flatMap(i => if (i > 0) Right(i + 1) else Left("0 or smaller")) === Right(2))
  }

  "left.flatMap" should "return Right(2)" in {
    val left = Left("error"): Either[String, Int]
    assert(left.flatMap(i => if (i > 0) Right(i + 1) else Left("0 or smaller")) === Left("error"))
  }

  "right1.flatMap" should "return Left('0 or smaller')" in {
    val right1 = Right(0)
    assert(right1.flatMap(i => if (i > 0) Right(i + 1) else Left("0 or smaller")) === Left("0 or smaller"))
  }

  // orElse

  "Right(0).orElse(Right(1))" should "return Right(0)" in {
    assert(Right(0).orElse(Right(1)) === Right(0))
  }

  "Left('error').orElse(Right(1))" should "return Right(1)" in {
    assert(Left("error").orElse(Right(1)) === Right(1))
  }

  // map2

  "right1.map2(right2)((a: Int, b: Double) => a.toString + b.toString)" should "return Right(12.0)" in {
    val right1 = Right(1)
    val right2 = Right(2.0)
    assert(right1.map2(right2)((a: Int, b: Double) => a.toString + b.toString) === Right("12.0"))
  }

  "left1.map2(right2)((a: Int, b: Double) => a.toString + b.toString)" should "return Right(12.0)" in {
    val left1 = Left("error1")
    val right2 = Right(2.0)
    assert(left1.map2(right2)((a: Int, b: Double) => a.toString + b.toString) === Left("error1"))
  }

  "right1.map2(left2)((a: Int, b: Double) => a.toString + b.toString)" should "return Right(12.0)" in {
    val right1 = Right(1)
    val left2 = Left("error2")
    assert(right1.map2(left2)((a: Int, b: Double) => a.toString + b.toString) === Left("error2"))
  }
}
