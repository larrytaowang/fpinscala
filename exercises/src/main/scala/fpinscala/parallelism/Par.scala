package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  /**
    * Exercise 4 Write a function to convert any function A => B to one that evaluates its result asynchronously
    */
  def asyncF[A, B](f: A => B): A => Par[B] = a => es => es.submit(() => f(a))

  /**
    * Exercise 5.1 Implement product() as primitives
    */
  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = es => es.submit(() => (fa(es).get, fb(es).get))

  /**
    * Exercise 5.2 Implement map() as primitives
    */
  def mapAsPrimitive[A, B](fa: Par[A])(f: A => B): Par[B] = es => es.submit(() => f(fa(es).get))

  /**
    * Exercise 5.3 Implement map2() in terms of product() and map()
    */
  def map2ViaMapAndProduct[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    mapAsPrimitive(product(a, b))((x: (A, B)) => f(x._1, x._2))

  /**
    * Exercise 6 Implement map in N parallel computations
    */
  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = l.map(asyncF(f))
    sequence(fbs)
  }

  /**
    * Exercise 7 Collect results of N parallel computations
    */
  def sequence[A](l: List[Par[A]]): Par[List[A]] = (es: ExecutorService) => {
    UnitFuture(l.map((a: Par[A]) => a(es).get))
  }

  /**
    * Exercise 8 Implement parFilter, which filters elements of a list in parallel
    */
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fbs = l.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(fbs))(_.flatten)
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  /**
    * Exercise 12 Fork without deadlock
    */
  def forkNoDeadlock[A](a: => Par[A]): Par[A] = ???

  /**
    * Exercise 14
    */
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /**
    * Exercise 15.1 Implement choiceN
    */
  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val index = run(es)(a).get
      choices(index)(es)
    }

  /**
    * Exercise 15.2 Implement choice in terms of choiceN
    */
  def choiceViaN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(x => if (x) 1 else 0))(List(t, f))

  /**
    * Exercise 16
    */
  def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] =
    es => {
      val key = run(es)(a).get
      choices(key)(es)
    }

  /**
    * Exercise 17.1 Implement chooser
    */
  def chooser[A, B](a: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val key = run(es)(a).get
      choices(key)(es)
    }

  /**
    * Exercise 17.2 Implement choice via Chooser
    */
  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(x => if (x) t else f)

  /**
    * Exercise 17.3 Implement choice via Chooser
    */
  def choiceNViaChooser[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(a)(x => choices(x))

  /**
    * Exercise 18 Implement join
    */
  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {}

}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
