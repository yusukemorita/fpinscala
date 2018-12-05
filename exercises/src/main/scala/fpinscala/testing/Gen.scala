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

case class Gen[A](sample: State[RNG,A]) {




}

object Gen {
  def unit[A](a: => A): Gen[A] = ???

  // 5, 10
  // 5, 6, 7, 8, 9
  // 0, 1, 2, 3, 4, 5
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val a: Rand[Int] = rng =>{
      val (i, nextRng) = RNG.nonNegativeLessThan(stopExclusive - start)(rng)
      (i + start, nextRng)
    }
    Gen(State(a))
  }
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

