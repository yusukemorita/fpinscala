package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
//  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
//    case Nil => 0 // The sum of the empty list is 0.
//    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
//  }
//
//  def product(ds: List[Double]): Double = ds match {
//    case Nil => 1.0
//    case Cons(0.0, _) => 0.0
//    case Cons(x,xs) => x * product(xs)
//  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    // matches here, results in x + y = 1 + 2 = 3
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def append2[A](l: List[A], z: List[A]): List[A] =
    foldRight(l: List[A], z: List[A])((newHead: A, resultList: List[A]) => Cons(newHead, resultList))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case list => Cons(h, list)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case num if num <= 0 => l
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(n, tail) if f(n) => dropWhile(tail, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // def length[A](l: List[A]): Int = foldRight(l, 0)((_, accumulator) => accumulator + 1)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
//    val func: B => B = b => b
    foldLeft(l, (b: B) => b) ( (g, a) => (b => g(f(a, b))) ) (z)
  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((accumulator, _) => accumulator + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((z: List[A], h: A) => Cons(h, z))
  // def reverse[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])((z: List[A], h: A) => Cons(h, z))

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  @annotation.tailrec
  def flatten1[A](l: List[List[A]]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(list, Nil) => list
      case Cons(list1, Cons(list2, remainingLists)) => flatten1(setHead(remainingLists, append(list1, list2)))
    }
  }

  def flatten[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append)

  def addOneToEachElement(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((head, tail) => Cons(head + 1, tail))
  }

  def doublesInListToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((head, tail) => Cons(head.toString, tail))
  }
}
