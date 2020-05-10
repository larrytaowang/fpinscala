package fpinscala.gettingstarted

import org.scalatest.FunSuite

class MyModuleTest extends FunSuite {

  test("testFib") {
    val expected = List(0, 1, 1, 2, 3, 5, 8)
    for ( i <- expected.indices) {
      assert(MyModule.fib(i) == expected(i))
    }
  }

}
