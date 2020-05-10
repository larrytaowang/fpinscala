package fpinscala.gettingstarted

import org.scalatest.FunSuite

class PolymorphicFunctionsTest extends FunSuite {

  test("testIsSorted") {
    val tests: Array[(Array[Int], Boolean)] = Array(
      (Array(), true),
      (Array(1), true),
      (Array(1, 2), true),
      (Array(1, 1, 2, 2), true),
      (Array(2, 1), false)
    )

    for ((input, expected) <- tests) {
      val gt = (x: Int, y: Int) => x.compare(y) > 0
      assert(PolymorphicFunctions.isSorted(input, gt) == expected)
    }
  }

}
