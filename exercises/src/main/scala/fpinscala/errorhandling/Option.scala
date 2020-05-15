package fpinscala.errorhandling


import java.util.regex.{Pattern, PatternSyntaxException}

import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

/**
  * Exercise 1 Implement all of the functions in trait Option
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 2 Implement variance in terms of mean and flatMap
    */
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs map (x => (x - m) * (x - m))))

  /**
    * Exercise 3 Write a generic function map2 that combines two Option values using a binary function.
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }

  /**
    * Exercise 4 Re-implement bothMatch in terms of map2
    */
  def bothMatchViaMap2[A, B, C](pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((f, g) => f(s) && g(s))

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches())

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case _: PatternSyntaxException => None
    }

  /**
    * Exercise 5 Write a function sequence that combines a list of Options into one option containing a list of all the
    * Some values in the original list. If the original list contains None even once, the result of the function should
    * be None.
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(List())
    case None :: _ => None
    case Some(x) :: xs => sequence(xs).map(x :: _)
  }

  /**
    * Exercise 6 Implement this function while only looks at the list once.
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => f(x) match {
      case None => None
      case Some(b) => traverse(xs)(f).map(b :: _)
    }
  }
}