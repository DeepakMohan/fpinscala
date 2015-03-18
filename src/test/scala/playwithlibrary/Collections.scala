package playwithlibrary

import org.scalatest.FunSuite

/**
 * Created by dmohan200 on 12/9/14 4:50 PM.
 */
class Collections extends FunSuite {

  def _sum(input: Seq[Int]): Int = {
    input.foldLeft(0)((a, b) => a + b)
  }

  def sum(input: Seq[Int]): Int = {
    if(input.size <=1) input.headOption getOrElse 0
    else {
      val (l, r) = input.splitAt(input.size / 2)
      sum(l) + sum(r)
    }
  }

  test ("sum") {
    val input = Seq(1,2,3,4,5,6,7,8,9,10)
    val sumVal = _sum(input)
    assert(55 == sumVal)
  }



}
