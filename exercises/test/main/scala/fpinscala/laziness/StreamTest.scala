package fpinscala.laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("testToList") {
    val tests = Array(
      (Stream(), List()),
      (Stream(1, 2), List(1, 2)),
      (Stream(1, 2, 3), List(1, 2, 3))
    )

    for ((input, expected) <- tests) {
      assert(input.toList == expected)
    }
  }

  test("testTake") {
    val tests = Array(
      (Stream(1, 2, 3, 4), 3, Stream(1, 2, 3)),
      (Stream(), 5, Stream()),
      (Stream(1), 1, Stream(1))
    )

    for ((input, n, expected) <- tests) {
      assert(input.take(n).toList == expected.toList)
      assert(input.takeViaUnfold(n).toList == expected.toList)
    }
  }

  test("testTakeWhile") {
    val f = (x: Int) => x % 2 == 0
    val tests = Array(
      (Stream(1, 2, 3, 4), f, Stream()),
      (Stream(2, 3, 4), f, Stream(2)),
      (Stream(2, 4, 6), f, Stream(2, 4, 6)),
      (Stream(2, 4, 1, 6), f, Stream(2, 4))
    )

    for ((input, f, expected) <- tests) {
      assert(input.takeWhile(f).toList == expected.toList)
      assert(input.takeWhileViaFoldRight(f).toList == expected.toList)
      assert(input.takeWhileViaUnfold(f).toList == expected.toList)
    }
  }

  test("testForAll") {
    val f = (x: Int) => x % 2 == 0
    val tests = Array(
      (Stream(1, 2, 3, 4), f, false),
      (Stream(2, 4, 6), f, true),
      (Stream(1, 2, 4, 6), f, false)
    )

    for ((input, f, expected) <- tests) {
      assert(input.forAll(f) == expected)
    }
  }

  test("testMap") {
    val tests = Array(
      (Stream(), Stream()),
      (Stream(1), Stream(2)),
      (Stream(1, 2), Stream(2, 3)),
      (Stream(1, 2, 3), Stream(2, 3, 4))
    )

    for ((s1, expected) <- tests) {
      assert(s1.map(_ + 1).toList == expected.toList)
      assert(s1.mapViaUnfold(_ + 1).toList == expected.toList)
    }
  }

  test("testFilter") {
    val f = (x: Int) => x % 2 == 0
    val tests = Array(
      (Stream(1, 2, 3, 4), f, Stream(2, 4)),
      (Stream(), f, Stream())
    )

    for ((input, f, expected) <- tests) {
      assert(input.filter(f).toList == expected.toList)
    }
  }

  test("testAppend") {
    val tests = Array(
      (Stream(1, 2, 3, 4), Stream(1, 2, 3, 4), Stream(1, 2, 3, 4, 1, 2, 3, 4)),
      (Stream(), Stream(1, 2, 3, 4), Stream(1, 2, 3, 4))
    )

    for ((s1, s2, expected) <- tests) {
      assert(s1.append(s2).toList == expected.toList)
    }
  }

  test("testFlatMap") {
    val f = (x: Int) => Stream(x, x + 1)
    val tests = Array(
      (Stream(), Stream()),
      (Stream(1), Stream(1, 2)),
      (Stream(1, 2), Stream(1, 2, 2, 3)),
      (Stream(1, 2, 3), Stream(1, 2, 2, 3, 3, 4))
    )

    for ((s1, expected) <- tests) {
      assert(s1.flatMap(f).toList == expected.toList)
    }
  }

  test("testConstant") {
    assert(Stream.constant(5).take(3).toList == List(5, 5, 5))
    assert(Stream.constantViaUnfold(5).take(3).toList == List(5, 5, 5))
    assert(Stream.onesViaUnfold().take(3).toList == List(1, 1, 1))
  }

  test("testFrom") {
    val count = 5
    val tests = Array(
      (1, List(1, 2, 3, 4, 5)),
      (2, List(2, 3, 4, 5, 6)),
      (3, List(3, 4, 5, 6, 7)),
      (4, List(4, 5, 6, 7, 8))
    )

    for ((n, expected) <- tests) {
      assert(Stream.from(n).take(count).toList == expected)
      assert(Stream.fromViaUnfold(n).take(count).toList == expected)
    }
  }

  test("testFibs") {
    assert(Stream.fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    assert(Stream.fibsViaUnfold().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("testZip") {
    val tests = Array(
      (Stream(), Stream("a"), Stream()),
      (Stream(1), Stream("a"), Stream((1, "a"))),
      (Stream(1, 2), Stream("a", "b"), Stream((1, "a"), (2, "b"))),
      (Stream(1, 2), Stream("a", "b", "c"), Stream((1, "a"), (2, "b")))
    )

    for ((s1, s2, expected) <- tests) {
      assert(s1.zipViaUnfold(s2).toList == expected.toList)
    }
  }

  test("testZipAll") {
    val tests = Array(
      (Stream(), Stream("a"), Stream((None, Some("a")))),
      (Stream(1), Stream("a"), Stream((Some(1), Some("a")))),
      (Stream(1, 2), Stream("a", "b"), Stream((Some(1), Some("a")), (Some(2), Some("b")))),
      (Stream(1, 2), Stream("a", "b", "c"), Stream((Some(1), Some("a")), (Some(2), Some("b")), (None, Some("c"))))
    )

    for ((s1, s2, expected) <- tests) {
      assert(s1.zipAllViaUnfold(s2).toList == expected.toList)
    }
  }

  test("testStartWith") {
    val tests = Array(
      (Stream(), Stream(1), false),
      (Stream(1), Stream(1), true),
      (Stream(1, 2, 3), Stream(1, 2), true),
      (Stream(1, 2, 3), Stream(2, 3), false)
    )

    for ((s1, s2, expected) <- tests) {
      assert(s1.startsWith(s2) == expected)
    }
  }

  test("testDrop") {
    val tests = Array(
      (Stream(1, 2, 3), 1, Stream(2, 3)),
      (Stream(1, 2, 3), 4, Stream()),
      (Stream(1, 2, 3), 2, Stream(3))
    )

    for ((s, n, expected) <- tests) {
      assert(s.drop(n).toList == expected.toList)
    }
  }

  test("testTails") {
    val tests = Array(
      (Stream(1, 2, 3), List(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream.empty))
    )

    for ((s, expected) <- tests) {
      assert(s.tails().toList.map(_.toList) == expected.map(_.toList))
    }
  }

  test("testScanRight") {
    val add: (Int, => Int) => Int = _ + _

    val tests = Array(
      (Stream(1, 2, 3), 0, add, List(6, 5, 3, 0))
    )

    for ((s, start, f, expected) <- tests) {
      assert(s.scanRight(start)(f).toList == expected)
    }
  }
}
