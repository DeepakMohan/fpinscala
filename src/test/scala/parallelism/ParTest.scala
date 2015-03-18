package parallelism

import java.net.URL
import java.util.concurrent.{Executors, ExecutorService}


import org.scalatest.FunSuite
import parallelism.Par._

/**
 * Created by dmohan200 on 3/3/15 11:02 PM.
 */
class ParTest extends FunSuite {

  val executorService: ExecutorService = Executors.newCachedThreadPool()

  def whoAmI(i: Int) = {
    Thread.sleep(i)
    (i, Thread.currentThread().getName)
  }

  /**
   * EXERCISE 7.4: using lazyUnit, write a function to convert any function A => B to one that evaluates its result asynchronously.
   */
  test("asyncFunction") {

    val asyncWhoAmI = asyncF(whoAmI)
    println(asyncWhoAmI(200)(executorService).get)
  }

  test("map2_withTimeout") {
    val executable = map2_withTimeout(lazyUnit(whoAmI(100)), lazyUnit(whoAmI(200)), 300L)((a, b) => ((a._1 + b._1), ("Logical Threads: " + a._2 + " & " + b._2)))
    val res = executable(executorService)
    println(res.get())

    val executable2 = map2_withTimeout(lazyUnit(whoAmI(500)), lazyUnit(whoAmI(200)), 300L)((a, b) => ((a._1 + b._1), ("Logical Threads: " + a._2 + " & " + b._2)))
    val res2 = executable2(executorService)
    println(res2.get())
  }

  test("parList to sorted parList") {
    val sorted = sortPar(lazyUnit(List(1,2,3,-10)))
    val sortedList = sorted(executorService)
    println(sortedList.get)
  }

  test("EXERCISE 7.5: parMap and sequence") {
    val input = List("https://www.google.com/", "https://www.yahoo.com/", "http://amazon.com", "http://www.comcast.com/")
    val pingables = parMap(input) {
      a =>
        val start = System.currentTimeMillis
        scala.io.Source.fromURL(a, "UTF-8")
        System.currentTimeMillis - start
    }
    val pingInMs = pingables(executorService)
    val pings = pingInMs.get()
    println(pings.zipWithIndex)
    assert(pings.size == 4)
  }

  test("EXERCISE 7.6: parFilter") {
    val input = List("https://www.google.com/", "https://www.yahoo.com/", "http://amazon.com", "http://www.comcast.com/")
    val pingables = parFilter(input) {
      a =>
        val start = System.currentTimeMillis
        scala.io.Source.fromURL(a, "UTF-8")
        val elapsed = System.currentTimeMillis - start
        elapsed < 10
    }

    val pingInMs = pingables(executorService)
    val pings = pingInMs.get()
    println(List("a","b","c","a","d","c","b").zipWithIndex)
    assert(pings.size == 0)
  }
}
