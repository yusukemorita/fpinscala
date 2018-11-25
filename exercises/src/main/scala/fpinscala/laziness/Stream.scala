package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toList: List[A] = {
    @annotation.tailrec
    def go(list: List[A], s: Stream[A]): List[A] = s match {
      case Cons(h, t) => go(h() :: list, t())
      case Empty => list
    }

    go(Nil, this).reverse
  }

  // since the listBuffer never escapes the toListWithBuffer method,
  // the function remains pure.
  // ie, buf has no effect on the outside of the toListWithBuffer function.
  def toListWithListBuffer: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case Empty => buf.toList
    }

    go(this)
  }


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, tail) if n > 0 => Cons(h, () => tail().take(n - 1))
    case Cons(_, _) if n <= 0 => empty
    case empty => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 0 => tail().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileByFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((x, acc) => if (p(x)) cons(x, acc) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, acc) => acc && p(x))

  def headOption: Option[A] = foldRight(None: Option[A])((x, _) => Some(x))

  // 5.7 map, filter, append, flatMap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, acc) => if (f(a)) cons(a, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight(s)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, acc) => f(a) append acc)

  def mapByUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)) {
    case (nn, Cons(_, _)) if nn <= 0 => None
    case (nn, Cons(h, t)) => Some((h(), (nn - 1, t())))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C]= unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s)) {
    case (Empty, Empty) => None
    case (s1, s2) => Some((s1.headOption, s2.headOption), (s1.drop(1), s2.drop(1)))
  }

  def startsWith[B](s: Stream[B]): Boolean = zipWith(s)((a, b) => a == b).forAll(_ == true)

  def tails: Stream[Stream[A]] = cons(this, unfold(this) {
    case Cons(_, t) => Some(t(), t())
    case _ => None
  })

  def scanRight1[B >: A](acc: B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(acc)(f))

  def scanRight2[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z))((a, zz) => zz.headOption match {
    case Some(h) => cons(f(a, h), zz)
    case None => empty
  })

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(n: Int, nn: Int): Stream[Int] = cons(n, go(nn, n + nn))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def fibsByUnfold: Stream[Int] = unfold((0, 1)){ case (a: Int, b: Int) => Some(a, (b, a + b))}

  def fromByUnfold(i: Int): Stream[Int] = unfold(i)((a: Int) => Some((a, a + 1)))

  def constantByUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  val onesByUnfold: Stream[Int] = constantByUnfold(1)

}