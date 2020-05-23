package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class ParTest extends FunSuite with BeforeAndAfterEach {
  val threadPoolSize = 10
  var executor: ExecutorService = _

  override def beforeEach() {
    executor = Executors.newFixedThreadPool(threadPoolSize)
  }

  test("testParMap") {
    val f = (x: Int) => x * 2
    val listSize = threadPoolSize * 2
    val unitValue = 1
    val parsList = List.fill(listSize)(unitValue)

    val result = Par.run(executor)(Par.parMap(parsList)(f)).get()
    assert(result == List.fill(listSize)(unitValue).map(f))
  }

  test("testSequence") {
    val listSize = threadPoolSize * 2
    val unitValue = 1
    val parsList = List.fill(listSize)(Par.unit(unitValue))

    val result = Par.run(executor)(Par.sequence(parsList)).get()
    assert(result == List.fill(listSize)(unitValue))
  }

  test("testParFilter") {
    val f = (x: Int) => x % 2 == 0
    val list = List.range(0, threadPoolSize * 2)

    val result = Par.run(executor)(Par.parFilter(list)(f)).get()
    assert(result == list.filter(f))
  }

  test("testChoiceN") {
    val list = List.range(0, threadPoolSize * 2)
    val choices = list.map(Par.asyncF(x => x))
    for (i <- 0 to threadPoolSize * 2 - 1) {
      val resultPar = Par.choiceN(Par.unit(i))(choices)
      val result = Par.run(executor)(resultPar).get
      assert(result == list(i))
    }
  }

}
