package fpinscala.laziness

import Stream.{unfold, _}
import fpinscala.gettingstarted.MyModule

trait Stream[+A] {

  /**
    * @tparam B The arrow `=>` in front of the argument type `B` means that the function `f` takes its second
    *           argument by name and may choose not to evaluate it.
    */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /**
    * Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`,
    * `b` will never be evaluated and the computation terminates early.
    */
  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /**
    * Exercise 1 Write a function to convert a Stream to a List, which will force its evaluation
    */
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  /**
    * Exercise 2 Write a function take for returning the first n elements of a stream
    */
  def take(n: Int): Stream[A] = (n, this) match {
    case (i, _) if i <= 0 => Empty
    case (_, Empty) => Empty
    case (_, Cons(h, t)) => cons(h(), t().take(n - 1))
  }

  /**
    * Exercise 3 Write a function that returns all starting elements of a Stream that match the given predicate
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  /**
    * Exercise 4 Checks that all elements in the Stream match a given predicate. Your implementation should terminate
    * the traversal as soon as it encounters a non-matching value
    */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

  /**
    * Exercise 5 Use foldRight to implement takeWhile
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (!p(a)) Empty else cons(a, acc))

  /**
    * Exercise 6.1 Implement map using foldRight
    */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

  /**
    * Exercise 6.2 Implement filter using foldRight
    */
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, acc) => if (f(h)) cons(h, acc.filter(f)) else acc)

  /**
    * Exercise 6.3 Implement append using foldRight
    */
  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a, acc) => cons(a, acc))

  /**
    * Exercise 6.4 Implement flatMap using foldRight
    */
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, acc) => f(a).append(acc))

  /**
    * Exercise 12.1  Use unfold to implement map.
    */
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  }

  /**
    * Exercise 12.2  Use unfold to implement take.
    */
  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  /**
    * Exercise 12.3  Use unfold to implement takeWhile.
    */
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(h, t) => if (p(h())) Some(h(), t()) else None
  }

  /**
    * Exercise 12.4 Use unfold to implement zip.
    */
  def zipViaUnfold[B](s: Stream[B]): Stream[(A, B)] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
    case _ => None
  }

  /**
    * Exercise 12.5 Use unfold to implement zipAll. The zipAll function should continue the traversal as long as
    * either stream has more element - it uses Option to indicate whether each stream has been exhausted.
    */
  def zipAllViaUnfold[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s)) {
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty[A], t()))
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty[B]))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case _ => None
  }

  /**
    * Create a new Stream[A] from this, but ignore the n first elements. This can be achieved by recursively calling
    * drop on the invoked tail of a cons cell. Note that the implementation is also tail recursive.
    */
  def drop(n: Int): Stream[A] = (this, n) match {
    case (Empty, _) => empty[A]
    case (_, n) if n <= 0 => this
    case (Cons(_, t), n) => t().drop(n - 1)
  }

  /**
    * Exercise 13 Implement startsWith using functions you've written. It should check if one Stream is a prefix of
    * another.
    */
  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
    case _ => false
  }

  /**
    * Exercise 14 Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input
    * sequence, starting with the original Stream.
    */
  def tails(): Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case x@Cons(_, t) => Some((x, t()))
  } append Stream(empty)

  /**
    * Exercise 15 Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the
    * intermediate results
    */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(empty[B])((a, acc) => {
      lazy val p1 = acc
      cons(f(a, acc.headOption.getOrElse(z)), p1)
    }) append Stream(z)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }
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

  /**
    * Exercise 7 Generalize ones slightly to the function constant which returns an infinite Stream of a given value
    */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
    * Exercise 8 Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, etc
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * Exercise 9 Write a function fibs that generates the infinite stream of Fibonacci numbers
    */
  def fibs(): Stream[Int] = {
    def go(i: Int): Stream[Int] = cons(MyModule.fib(i), go(i + 1))

    go(0)
  }

  /**
    * Exercise 10 A general stream building function. It takes an initial state, and a function for producing both
    * the next state and the next value in the generated stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  /**
    * Exercise 11.1 Write fibs in terms of unfold
    */
  def fibsViaUnfold(): Stream[Int] = unfold(0)(n => Some((MyModule.fib(n), n + 1)))

  /**
    * Exercise 11.2 Write from in terms of unfold
    */
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

  /**
    * Exercise 11.3 Write constant in terms of unfold
    */
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(i => Some(i, i))

  /**
    * Exercise 11.4 Write ones in terms of unfold
    */
  def onesViaUnfold(): Stream[Int] = unfold(1)(_ => Some(1, 1))
}

