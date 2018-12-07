package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.RNG.Rand

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean = ???
  def &&(p: Prop): Prop = new Prop {
    override def check = Prop.this.check && p.check
  }
}

object Prop {
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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

trait SGen[+A] {

}

