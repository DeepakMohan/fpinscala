package statemodifiers

import org.scalatest.FunSuite

/**
 * Created by dmohan200 on 12/24/14 4:56 PM.
 */
class RandomNumberGeneratorTest extends FunSuite {

  import statemodifiers.SimpleRNG._

  val rng = SimpleRNG(1234546)

  test("random number") {
    val r1 = SimpleRNG(123)
    val (r, r2) = r1.nextInt
    assert(r2 match {
      case SimpleRNG(3101433181802L) => true
      case _ => false
    })
    assert(r == 47324114)
  }

  test("EXERCISE 6.1: nonNegativeInt") {
    val (r, r2) = SimpleRNG.nonNegativeInt(rng)
    assert(r > 0)
    assert(r == 1751154232)
    assert(r2 == SimpleRNG(166711332944533l))
  }

  test("EXERCISE 6.2: double 0-1") {
    val (d, r) = double(rng)
    assert(d > 0 && d < 1)
  }

  test("EXERCISE 6.3: IntDouble Pair") {
    val ((i, d), _) = intDouble(rng)
    assert(i == -1751154233)
    assert(d == -1.7511542321845553E9)
  }

  test("EXERCISE 6.3: DoubleInt Pair") {
    val ((d, i), _) = doubleInt(rng)
    assert(d == -1.7511542321845553E9)
    assert(i == -1751154233)
  }

  test("EXERCISE 6.3: double3") {
    val ((d1, d2, d3), _) = double3(rng)
    assert(d1 == -1.7511542321845553E9)
    assert(d2 == 1.5158423127058692E9)
    assert(d3 == -1.3512641883707685E9)
  }

  test("EXERCISE 6.4: list of random integers") {
    val (randomIntegers, _) = ints(4)(rng)
    val (randomIntegerSeq, _) = ints_1(4)(rng)
    assert(List(-1751154233, 1515842312, -1351264189, 282151985) == randomIntegers)
    assert(List(1751154232, 1515842312, 1351264188, 282151984) == randomIntegerSeq)
  }

  test("nonNegativeEven") {
    //map(nonNegativeInt)(n => n - n % 2)
    val (i, n) = nonNegativeEvenInt(rng)
    val (i2, _) = nonNegativeEvenInt(n)
    assert(i == 1751154232)
    assert(i2 == 1515842312)
  }

  test("EXERCISE 6.5: double using map") {
    val (d, _) = double_1 (rng)
    assert(d > 0)
    assert(1.7511542328154447E9 == d)
  }

  test("EXERCISE 6.6: map2") {
    val ((i, d), r: RandomNumberGenerator) = map2(nonNegativeInt, double)((a,b) => (a,b))(rng)
    assert(i == 1751154232)
    assert(d == 0.7058690823614597)
    assert(r == SimpleRNG(99342241779580L))
  }

  test("EXERCISE 6.7: sequence") {
    val rngs: List[RNG[Int]] = List.fill(5)(nonNegativeEvenInt)
    val seq = sequence(rngs)
    assert(seq(rng) == (List(1751154232, 1515842312, 1351264188, 282151984, 154172190),SimpleRNG(271371148033001L)))
  }

  test("EXERCISE 6.4: list of random integers using sequence") {
    val (randomIntegers, _) = ints(4)(rng)
    val (randomIntegerSeq, _) = ints_1(4)(rng)
    assert(List(-1751154233, 1515842312, -1351264189, 282151985) == randomIntegers)
    assert(List(1751154232, 1515842312, 1351264188, 282151984) == randomIntegerSeq)
  }

  test("nonNegativeLessThan brute force method") {
    val (i, r) = nonNegativeLessThan(100)(rng)
    assert(i == 32)
    assert(i < 100)

    val (i2, r2) = nonNegativeIntLessThan(100)(rng)
    assert(i2 == 32)
    assert(i2 < 100)
  }


}
