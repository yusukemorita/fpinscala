package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue + 1))

  def nonNegativeInt1(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, nextRng) if n >= 0 => (n, nextRng)
    case (_, nextRng) => nonNegativeInt1(nextRng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    (if (n < 0) -(n+1) else n, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (d1, rng2) = double(rng1)
    ((i1, d1), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), nextRng) = intDouble(rng)
    ((d, i), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, r: RNG, list: List[Int]): (List[Int], RNG) = {
      if (c <= 0) {
        (list, r)
      } else {
        val (int, nextRng) = r.nextInt
        go(c - 1, nextRng, int :: list)
      }
    }
    go(count, rng, Nil)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(functionList: List[Rand[A]], resultList: List[A], r: RNG): (List[A], RNG) = functionList match {
      case h :: t =>
        val (a, nextRng) = h(r)
        go(t, a :: resultList, nextRng)
      case Nil => (resultList, r)
    }

    rng => go(fs, Nil, rng)
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs.foldRight((Nil: List[A], rng))((rand, acc) => rand(acc._2) match {
    case (a, nextRng) => (a :: acc._1, nextRng)
  })

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((randA, randListA) => map2(randA, randListA)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {

  def map1[B](f: A => B): State[S, B] = State(state => {
    val (a, rng2) = this.run(state)
    (f(a), rng2)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(state => {
    val (a, rng1) = run(state)
    f(a).run(rng1)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def unit[A, S](a: A): State[S, A] = State(state => (a, state))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[List[A], S](List()))((stateA, stateOfListA) => stateA.map2(stateOfListA)((a, b) => a::b))
}
