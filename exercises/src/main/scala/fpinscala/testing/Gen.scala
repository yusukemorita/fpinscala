package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.state.RNG.Rand
import Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (TestCases,RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (testCases, rng) => {
      this.run(testCases, rng) match {
        case Passed => p.run(testCases, rng)
        case x => x
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (testCases, rng) => {
      this.run(testCases, rng) match {
        case Falsified(_, _) => p.run(testCases, rng)
        case x => x
      }
    }
  }

}

sealed trait Result { def isFalsified: Boolean }
case object Passed extends Result { def isFalsified = false }

case class Falsified( failure: FailedCase, successes: SuccessCount ) extends Result { def isFalsified = true }

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  type TestCases = Int
  type Result = Either[(FailedCase, SuccessCount), SuccessCount]

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n: Int, rng: RNG) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }

      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[A](sample: State[RNG,A]){
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap2[B](f: A => Gen[B]): Gen[B] = Gen(State{ rng =>
    val (i, nextRng) = sample.run(rng)
    f(i).sample.run(nextRng)
  })

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(i => Gen.listOfN(i, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a): State[RNG, A])

  def boolean: Gen[Boolean] = {
    val a: Rand[Boolean] = rng => {
      val (i, nextRng) = RNG.nonNegativeInt(rng)
      (i % 2 == 0, nextRng)
    }
    Gen(State(a))
  }

  def boolean2: Gen[Boolean] = Gen(State(RNG.boolean))


  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    def go(nn: Int, r: RNG, l: List[A]): (List[A], RNG) = {
      nn match {
        case i if i <= 0 => (l, r)
        case i =>
          val (h, nextRng) = g.sample.run(r)
          go(i - 1, nextRng, h :: l)
      }
    }

    Gen(
      State(rng => go(n, rng, List[A]()))
    )
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  // 5, 10
  // 5, 6, 7, 8, 9
  // 0, 1, 2, 3, 4

  def chooseV2(start: Int, stopExclusive: Int): Gen[Int] = {
    val a: Rand[Int] = rng =>{
      val (i, nextRng) = RNG.nonNegativeLessThan(stopExclusive - start)(rng)
      (i + start, nextRng)
    }
    Gen(State(a))
  }

  def chooseTuple(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    listOfN(2, choose(start, stopExclusive)).map(i => (i.head, i(1)))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start - 1)).map(i => i + start))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(b => if (b) g1 else g2)

  def double: Gen[Double] = Gen(State(RNG.double))

  def weightedBoolean(d1: Double, d2: Double): Gen[Boolean] = {
    val probability = d1 / ( d1 + d1 )
    Gen(State(RNG.double)).map(d => if (d < probability) true else false)
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    weightedBoolean(g1._2, g2._2).flatMap(b => if (b) g1._1 else g2._1)
  }

  def optionGen[A](gen: Gen[A]): Gen[Option[A]] = {
    gen.map(Some(_))
  }

  def unOptionGen[A](gen: Gen[Option[A]]): Gen[A] = {
    gen.map {
      case Some(a) => a
      case None => ???
    }
  }

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[+A](forSize: Int => Gen[A])
