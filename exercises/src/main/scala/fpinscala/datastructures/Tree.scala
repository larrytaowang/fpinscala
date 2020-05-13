package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  /**
    * Exercise 25 Write a function size that counts the number of nodes in a tree
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /**
    * Exercise 26 Write a function that returns the maximum element in a Tree
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  /**
    * Exercise 26 Write a function that returns the maximum path length from the root of a tree to any leaf
    */
  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => depth(l).max(depth(r)) + 1
  }

  /**
    * Exercise 27 Write a function map that modifies each element in a tree with a given function
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
    * Exercise 29 Write a function fold that abstracts over the similarities of size, maximum, depth
    */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold(t: Tree[Int]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthViaFold(t: Tree[Int]): Int = fold(t)(_ => 0)(1 + _ max _)
}