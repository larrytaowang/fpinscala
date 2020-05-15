package fpinscala.errorhandling

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  test("testVariance") {
    val tests = Array(
      (Seq(), None),
      (Seq(1.0, 1.0, 1.0), Some(0)),
      (Seq(1.0, 2.0, 3.0), Some(0.6666666666666666))
    )

    for ((l, expected) <- tests) {
      assert(Option.variance(l) == expected)
    }
  }

  test("testTraverse") {
    val f = (x: Int) => if (x % 2 == 0) None else Some(x)
    val tests = Array(
      (List(), Some(Nil)),
      (List(1, 1, 1), Some(List(1, 1, 1))),
      (List(1, 1, 2), None)
    )

    for ((l, expected) <- tests) {
      assert(Option.traverse(l)(f) == expected)
    }
  }

  test("testSequence") {
    val tests = Array(
      (List(), Some(Nil)),
      (List(None, Some(1)), None),
      (List(Some(1), Some(2)), Some(List(1, 2)))
    )

    for ((l, expected) <- tests) {
      assert(Option.sequence(l) == expected)
    }
  }

}
