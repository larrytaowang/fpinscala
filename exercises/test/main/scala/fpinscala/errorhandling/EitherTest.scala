package fpinscala.errorhandling

import org.scalatest.FunSuite

class EitherTest extends FunSuite {

  test("testTraverse") {
    val f = (x: Int) => if (x % 2 == 0) Left() else Right(x)
    val tests = Array(
      (List(), Right(Nil)),
      (List(1, 1, 1), Right(List(1, 1, 1))),
      (List(1, 1, 2), Left())
    )

    for ((l, expected) <- tests) {
      assert(Either.traverse(l)(f) == expected)
    }
  }

  test("testSequence") {
    val tests = Array(
      (List(), Right(Nil)),
      (List(Left(), Right(1)), Left()),
      (List(Right(1), Right(2)), Right(List(1, 2)))
    )

    for ((l, expected) <- tests) {
      assert(Either.sequence(l) == expected)
    }
  }
}
