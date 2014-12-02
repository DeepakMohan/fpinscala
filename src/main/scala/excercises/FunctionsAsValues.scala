package excercises

import scala.annotation.tailrec

/**
 * Created by dmohan200 on 11/17/14.
 */
object FunctionsAsValues extends App {

  def good(i: Int): Double = {
    i * 0.12323
  }

  def better(i: Int): Double = {
    i * 0.56563
  }

  def best(i: Int): Double = {
    i * 0.99234
  }

  def idf(i: Int, boostBy: (Int => Double)) = {
    i + boostBy(i)
  }

  // collection of functions
  val boost: List[Int => Double] = List(good, better, best)
  val scores = List(10, 20)

  val relevancy =
    for (s <- scores;
      b <- boost
    )
    yield idf(s, b) //functions as parameter

  relevancy map(println _)
}

object Factorial extends App {

  def factorial(n: Int): Int = {
    n * factorial(n - 1)
  }

  def factorialTR(n: Long): Long = {

    @tailrec
    def loop(n: Long, accumulator: Long): Long = {
      if(n <= 0) accumulator
      else loop(n-1, n*accumulator)
    }
    loop(n, 1L)
  }

  val ns = List(10, 5)
  val fs: List[Long => Long] = List(factorialTR) // factorial => stackoverflowexception

  val factorials: List[Long] = for (n <- ns; f <- fs) yield f(n)

  factorials map println _
}