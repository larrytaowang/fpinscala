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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * Exercise 1 Generate a random positive integer. Ignore the case of Int.Minvalue
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (n.abs, nextRNG)
  }

  /**
    * Exercise 2 Write a function to generate a Double between 0 and 1, not including 1.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (n / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  /**
    * Exercise 3.1 Write a function to generate an (Int, Double) pair
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  /**
    * Exercise 3.2 Write a function to generate an (Double, Int) pair
    */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  /**
    * Exercise 3.3 Write a function to generate an (Double, Double, Double) pair
    */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  /**
    * Exercise 4 Write a function to generate a list of random integers
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(i: Int, newRng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (i == count) (Nil, newRng)
      else {
        val (x, r2) = newRng.nextInt
        go(i + 1, r2, x :: acc)
      }
    }

    go(0, rng, Nil)
  }

  /**
    * Exercise 5 Use map to generate an Int between 0 and n, inclusive
    */
  def positiveMax(n: Int): Rand[Int] = map(nonNegativeInt)(_ % (n + 1))

  /**
    * Exercise 6 Use map to reimplement RNG.double in a more elegant way
    */
  def doubleViaMap(): Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
    * Exercise 7 A combinator map2 that can combine two RNG actions into one using a binary rather than unary function
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  /**
    * Exercise 8.1 Combining a list of transitions into a single transition
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
    case Nil => (Nil, rng)
    case x :: xs =>
      val (a, r1) = x(rng)
      map(sequence(xs))(a :: _)(r1)
  }

  /**
    * Exercise 8.2 Implement ints with sequence
    */
  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  /**
    * Exercise 9.1 Implement flatMap
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  /**
    * Exercise 9.2 Reimplement nonNegativeInt via flatMap
    */
  def nonNegativeIntViaFlatMap(): Rand[Int] = flatMap(int)(x => {
    if (x != Int.MinValue) unit(x)
    else nonNegativeIntViaFlatMap()
  })

  /**
    * Exercise 10.1 Reimplement map in terms of flatMap
    */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  /**
    * Exercise 10.2 Reimplement map in terms of flatMap
    */
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S, +A](run: S => (A, S)) {
  /**
    * Exercise 11.2
    */
  def map[B](f: A => B): State[S, B] = flatMap(a => State(s => (f(a), s)))

  /**
    * Exercise 11.3
    */
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  /**
    * Exercise 11.4
    */
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = this.run(s)
    f(a).run(s1)
  })

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  /**
    * Exercise 11.1 This function should be placed in object State since this is used to create a State
    */
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  /**
    * Exercise 12.1
    */
  def get[S, A]: State[S, S] = State(s => (s, s))

  /**
    * Exercise 12.2
    */
  def set[S, A](s: S): State[S, Unit] = State(_ => ((), s))
}
