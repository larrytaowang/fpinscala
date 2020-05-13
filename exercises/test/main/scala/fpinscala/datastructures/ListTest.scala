package fpinscala.datastructures

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("testTail") {
    val tests = Array(
      (List(), List()),
      (List(1, 2), List(2)),
      (List(1, 2, 3), List(2, 3))
    )

    for ((input, expected) <- tests) {
      assert(List.tail(input) == expected)
    }
  }

  test("testDrop") {
    val tests = Array(
      (List(), 0, List()),
      (List(1, 2, 3), 1, List(2, 3)),
      (List(1, 2, 3), 2, List(3)),
      (List(1, 2, 3), 3, List())
    )

    for ((inputList, n, expected) <- tests) {
      assert(List.drop(inputList, n) == expected)
    }
  }

  test("testDropWhile") {
    val isOdd = (x: Int) => x % 2 == 1
    val tests = Array(
      (List(), isOdd, List()),
      (List(1, 2, 3), isOdd, List(2, 3)),
      (List(2, 3), isOdd, List(2, 3))
    )

    for ((inputList, f, expected) <- tests) {
      assert(List.dropWhile(inputList, f) == expected)
    }
  }

  test("testSetHead") {
    val tests = Array(
      (List(), 1, List(1)),
      (List(1, 2, 3), 1, List(1, 2, 3)),
      (List(1, 2, 3), 0, List(0, 2, 3))
    )

    for ((inputList, newHead, expected) <- tests) {
      assert(List.setHead(inputList, newHead) == expected)
    }
  }

  test("testInit") {
    val tests = Array(
      (List(), List()),
      (List(1), List()),
      (List(1, 2), List(1))
    )

    for ((inputList, expected) <- tests) {
      assert(List.init(inputList) == expected)
    }
  }

  test("testLengthAndLength2") {
    val tests = Array(
      (List(), 0),
      (List(1), 1),
      (List(1, 2), 2)
    )

    for ((inputList, expected) <- tests) {
      assert(List.lengthViaFoldRight(inputList) == expected)
      assert(List.lengthViaFoldLeft(inputList) == expected)
    }
  }

  test("testSum3") {
    val tests = Array(
      (List(), 0),
      (List(1), 1),
      (List(1, 2), 3)
    )

    for ((inputList, expected) <- tests) {
      assert(List.sumViaFoldLeft(inputList) == expected)
    }
  }

  test("testProduct3") {
    val tests = Array(
      (List(), 1.0),
      (List(1.0), 1.0),
      (List(1.0, 2.0), 2.0)
    )

    for ((inputList, expected) <- tests) {
      assert(List.productViaFoldLeft(inputList) == expected)
    }
  }

  test("testReverse") {
    val tests = Array(
      (List(), List()),
      (List(1), List(1)),
      (List(1, 2), List(2, 1))
    )

    for ((inputList, expected) <- tests) {
      assert(List.reverseViaFoldLeft(inputList) == expected)
    }
  }

  test("testAppend") {
    val tests = Array(
      (List(), List(), List()),
      (List(1), List(1), List(1, 1)),
      (List(1, 2), List(2, 1), List(1, 2, 2, 1))
    )

    for ((list1, list2, expected) <- tests) {
      assert(List.append(list1, list2) == expected)
    }
  }

  test("testConcatenate") {
    val tests = Array(
      (List(List(), List()), List()),
      (List(List(1), List(1)), List(1, 1)),
      (List(List(1, 2), List(3, 4), List(5, 6)), List(1, 2, 3, 4, 5, 6))
    )

    for ((lists, expected) <- tests) {
      assert(List.concatenate(lists) == expected)
    }
  }

  test("testAddOne") {
    val tests = Array(
      (List(), List()),
      (List(1), List(2)),
      (List(1, 2), List(2, 3))
    )

    for ((inputList, expected) <- tests) {
      assert(List.addOne(inputList) == expected)
    }
  }

  test("testDoubleToString") {
    val tests = Array(
      (List(), List()),
      (List(1.0), List("1.0")),
      (List(1.0, 2.0), List("1.0", "2.0"))
    )

    for ((inputList, expected) <- tests) {
      assert(List.doubleToString(inputList) == expected)
    }
  }

  test("testRemoveOdd") {
    val tests = Array(
      (List(), List()),
      (List(1), List()),
      (List(1, 2), List(2))
    )

    for ((inputList, expected) <- tests) {
      assert(List.removeOdd(inputList) == expected)
    }
  }

  test("testFlapMap") {
    val tests = Array(
      (List(1, 2, 3), (i: Int) => List(i, i), List(1, 1, 2, 2, 3, 3))
    )

    for ((inputList, f, expected) <- tests) {
      assert(List.flatMap(inputList)(f) == expected)
    }
  }

  test("testAddList") {
    val tests = Array(
      (List(1, 2, 3), List(1, 2, 3), List(2, 4, 6))
    )

    for ((l1, l2, expected) <- tests) {
      assert(List.addListPair(l1, l2) == expected)
      assert(List.addListPair1(l1, l2) == expected)
    }
  }

  test("testHasSubsequence") {
    val tests = Array(
      (List(1, 2, 3), List(1, 2), true),
      (List(1, 2, 3), List(1, 2, 3), true),
      (List(1, 2, 3), List(2, 3), true),
      (List(1, 2, 3), List(2, 3, 4), false)
    )

    for ((l, sub, expected) <- tests) {
      assert(List.hasSubsequence(l, sub) == expected)
    }
  }

}
