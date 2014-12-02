package excercises

/**
 * Created by dmohan200 on 11/4/14.
 *
 * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
 * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
 * previous two—the sequence begins 0, 1, 1, 2, 3, 5.
 * Your definition should use a local tail-recursive function.
 *
 * def fib(n: Int): Int
 */ 
object Fibonacci extends App {

  def computeNthFibonacci(n: Int): Long = {

    @annotation.tailrec
    def go(nth: Int, lastTwo: (Long, Long)): Long = {
      if(nth == n) {
        lastTwo._1 + lastTwo._2
      } else {
        go(nth+1, (lastTwo._2, lastTwo._1 + lastTwo._2))
      }
    }

    go(3, (0, 1))
  }

  println(computeNthFibonacci(700))
}
