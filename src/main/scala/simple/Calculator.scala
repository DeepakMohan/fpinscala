package simple

import org.scalatest.FunSuite

/**
 * Created by dmohan200 on 3/17/15 10:51 PM.
 */
class Calculator {

  def add(left: Int, right: Int): Int = {
    left + right
  }
}

class CalcRunner extends FunSuite {

  val calc = new Calculator

  test("simple addition") {
    val sum: Int = calc.add(100, 200)
    assert(sum == 300)

    val sum2: Int = calc.add(0, 200)
    assert(sum2 == 200)

    val sum3: Int = calc.add(-100, 200)
    assert(sum3 == 100)
  }
}