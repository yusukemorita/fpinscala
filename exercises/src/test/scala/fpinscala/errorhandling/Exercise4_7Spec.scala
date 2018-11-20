package fpinscala.errorhandling

import org.scalatest._

class Exercise4_7Spec extends FlatSpec {

  // sequence

  "List(right1, right2, right3))" should "return Right(List(1, 2, 3))" in {
    val right1 = Right(1)
    val right2 = Right(2)
    val right3 = Right(3)
    assert(Either.sequence(List(right1, right2, right3)) === Right(List(1, 2, 3)))
  }

  "List(left1, right2, right3))" should "return Right(List(1, 2, 3))" in {
    val left1 = Left("error1")
    val right2 = Right(2)
    val right3 = Right(3)
    assert(Either.sequence(List(left1, right2, right3)) === Left("error1"))
  }

  "List(left1, left2, right3))" should "return Right(List(1, 2, 3))" in {
    val left1 = Left("error1")
    val left2 = Left("error2")
    val right3 = Right(3)
    assert(Either.sequence(List(left1, left2, right3)) === Left("error1"))
  }

  // traverse

  "Either.traverse(List(right1, right2, right3))(f)" should "return Right(List(1, 2, 3))" in {
    val right1 = Right(1)
    val right2 = Right(2)
    val right3 = Right(3)
    val f: Either[String, Int] => Either[String, Int] = {
      case Right(aa) => Right(aa + 1)
      case Left(e) => Left(e)
    }

    assert(Either.traverse(List(right1, right2, right3))(f) === Right(List(2, 3, 4)))
  }

  "Either.traverse(List(left1, right2, right3))(f)" should "return Right(List(1, 2, 3))" in {
    val left1 = Left("error1")
    val right2 = Right(2)
    val right3 = Right(3)
    val f: Either[String, Int] => Either[String, Int] = {
      case Right(aa) => Right(aa + 1)
      case Left(e) => Left(e)
    }

    assert(Either.traverse(List(left1, right2, right3))(f) === Left("error1"))
  }

  "Either.traverse(List(left1, left2, right3))(f)" should "return Right(List(1, 2, 3))" in {
    val left1 = Left("error1")
    val left2 = Left("error2")
    val right3 = Right(3)
    val f: Either[String, Int] => Either[String, Int] = {
      case Right(aa) => Right(aa + 1)
      case Left(e) => Left(e)
    }

    assert(Either.traverse(List(left1, left2, right3))(f) === Left("error1"))
  }
}
