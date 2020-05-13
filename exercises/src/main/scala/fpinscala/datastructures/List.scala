package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /**
    * Exercise 2
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  /**
    * Exercise 3
    */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  /**
    * Exercise 4
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) t else l
  }

  /**
    * Exercise 5
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * Exercise 6 Implement init: returns a List consisting of all but the last element of a List
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
    * Exercise 9 Implement length with FoldRight
    */
  def lengthViaFoldRight[A](l: List[A]): Int = foldRight(l, 0)((_, y) => 1 + y)

  /**
    * Exercise 10 Implement tail-recursive foldLeft
    */
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * Exercise 11 Write Sum, Product, and length using foldLeft
    */
  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0)((x, y) => x + y)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => 1 + x)

  /**
    * Exercise 12 Write a function that returns the reverse of a List using a fold.
    */
  def reverseViaFoldLeft[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  /**
    * Exercise 13 Write foldLeft in terms of foldRight, and write foldRight in terms of foldLeft
    */
  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverseViaFoldLeft(as), z)((b, a) => f(a, b))

  /**
    * Exercise 14 Implement append in terms of foldRight
    */
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, acc) => Cons(x, acc))

  /**
    * Exercise 15 Concatenates a list of lists into a single list. Its runtime should be linear in the total length
    * of all lists
    */
  def concatenate[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((x, acc) => append(x, acc))

  /**
    * Exercise 16 Write a function that transforms a list of integers by adding 1 to each element
    */
  def addOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  /**
    * Exercise 17 Write a function that transforms a list of integers by adding 1 to each element
    */
  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  /**
    * Exercise 18 Write a function map, that generalize modifying each element in a list while maintaining the
    * structure of the list
    */
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  /**
    * Exercise 19 Write a function filter that removes elements from a list until they satisfy a given predicate. Use
    * it to remove all odd numbers from a List[Int]
    */
  def filter[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if !f(x) => xs
    case _ => l
  }

  def filterViaFold[A](l: List[A], f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def removeOdd(l: List[Int]): List[Int] = filter(l, (x: Int) => x % 2 == 0)

  /**
    * Exercise 20 Write a function flatMap
    */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B])((a, acc) => append(f(a), acc))

  /**
    * Exercise 21 Use flatMap to implement Filter
    */
  def filterViaFlatMap[A](l: List[A], f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) List(x) else Nil)

  /**
    * Exercise 22 Write a function that accepts two lists and constructs a new list by adding corresponding elements
    */
  def addListPair[A](l1: List[A], l2: List[A])(implicit wrapper: Numeric[A]): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(wrapper.plus(h1, h2), addListPair(t1, t2))
  }

  def addListPair1[A](l1: List[A], l2: List[A])(implicit wrapper: Numeric[A]): List[A] =
    reduceListPair(l1, l2)(wrapper.plus)

  /**
    * Exercise 23 Generalize Exercise 22
    */
  def reduceListPair[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), reduceListPair(t1, t2)(f))
  }

  /**
    * Exercise 24 Check whether a List contains another List as a subsequence
    */
  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Nil, _) => sub == Nil
    case (Cons(l1, l2), Cons(s1, s2)) => if (l1 == s1) hasSubsequence(l2, s2) else hasSubsequence(l2, sub)
  }
}
