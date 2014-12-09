package statemodifiers

/**
 * Created by dmohan200 on 12/24/14 4:50 PM.
 */
trait RandomNumberGenerator {
  def nextInt: (Int, RandomNumberGenerator)
}

case class SimpleRNG(seed: Long) extends RandomNumberGenerator {

  override def nextInt: (Int, RandomNumberGenerator) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (seed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {

  /**
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and
   * Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
   * Int.MinValue, which does not have a non-negative counterpart.
   *
   * @param rng RandomNumberGenerator
   */
  def nonNegativeInt(rng: RandomNumberGenerator): (Int, RandomNumberGenerator) = {
    val (n, next) = rng.nextInt
    if (n >= 0) (n, next)
    else (-(n+1), next)

    // n < 0 => if n == Int.minVal Int.maxVal else (n * -1)
  }

  /**
   * Write a function to generate a Double between 0 and 1, not including 1.
   *
   * @param rng RandomNumberGenerator
   * @return
   */
  def double(rng: RandomNumberGenerator): (Double, RandomNumberGenerator) = {
    val (n, next) = nonNegativeInt(rng)
    (n / (Integer.MAX_VALUE.toDouble + 1), next)
  }

  def intDouble(rng: RandomNumberGenerator): ((Int, Double), RandomNumberGenerator) = {
    val (n, next) = rng.nextInt
    ((n, n + double(rng)._1), next)
  }

  def doubleInt(rng: RandomNumberGenerator): ((Double, Int), RandomNumberGenerator) = {
    val ((i, d), next) = intDouble(rng)
    ((d, i), next)
  }

  def double3(rng: RandomNumberGenerator): ((Double, Double, Double), RandomNumberGenerator) = {
    val ((_, d1), n1) = intDouble(rng)
    val ((_, d2), n2) = intDouble(n1)
    val ((_, d3), n3) = intDouble(n2)
    ((d1, d2, d3), n3)
  }

  def ints(n: Int)(rng: RandomNumberGenerator): (List[Int], RandomNumberGenerator) = {
    if(n > 0) {
      val (i1, n1) = rng.nextInt
      if(n-1 > 0) {
        val (l, n2) = ints(n-1)(n1)
        (i1 :: l, n2)
      } else
        (List(i1), n1)
    } else {
      (List[Int](), rng)
    }
  }

  def ints_tr_chuck(count: Int)(rng: RandomNumberGenerator): (List[Int], RandomNumberGenerator) = {
    @annotation.tailrec
    def loop(accum: List[Int], prevRNG: RandomNumberGenerator, numToDo: Int): (List[Int], RandomNumberGenerator) = {
      if(numToDo == 0) (accum.reverse, prevRNG)
      else {
        val (anInt, nextRNG) = prevRNG.nextInt
        loop(anInt :: accum, nextRNG, numToDo - 1)
      }
    }
    loop(List(), rng, count)
  }

  /**
   *
   * @param n
   * @param rng
   * @return
   */
  def ints_1(n: Int)(rng: RandomNumberGenerator): (List[Int], RandomNumberGenerator) = {
    val rngs = List.fill(n)(nonNegativeEvenInt)
    sequence(rngs)(rng)
  }

  //type alias
  type RNG[+A] = RandomNumberGenerator => (A, RandomNumberGenerator)

  def unit[A](a: A): RNG[A] = {
    rng => (a, rng)
  }

  def map[A, B](s: RNG[A])(f: A => B): RNG[B] =
    rngA => {
      val (a, rngA2) = s(rngA)
      (f(a), rngA2)
    }

  /**
   * Write the implementation of map2 based on the following signature. This function
   * takes two actions, ra and rb, and a function f for combining their results, and returns
   * a new action that combines them
   * @param ra
   * @param rb
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[A, B, C](ra: RNG[A], rb: RNG[B])(f: (A, B) => C): RNG[C] = {
    rngA => {
      val (a, rngA2) = ra(rngA)
      val (b, rngB) = rb(rngA2)
      (f(a, b), rngB)
    }
  }

  /**
   * if we have an action that generates values of type A and an action
   * to generate values of type B, then we can combine them into one
   * action that generates pairs of both A and B

   * @param ra
   * @param rb
   * @tparam A
   * @tparam B
   * @return
   */
  def both[A, B](ra: RNG[A], rb: RNG[B]): RNG[(A,B)] = {
    map2(ra, rb)((a,b) => (a,b))
    //map2(ra, rb)((_,_))
  }

  def sequence[A](rngs: List[RNG[A]]): RNG[List[A]] = {
    rng =>
      rngs match {
        case r :: rs => map2(r, sequence(rs))(_ :: _)(rng)
        case _ => (List[A](), rng)
      }
  }

  def nonNegativeLessThan(n: Int): RNG[Int] = {
    rng =>
      val (i, r) = nonNegativeInt(rng)
      val mod = i % n
      if(i + (n-1) - mod > 0) {
        (mod, r)
      } else {
        nonNegativeLessThan(n)(rng)
      }
  }

  def flatMap[A, B](f: RNG[A])(g: A => RNG[B]): RNG[B] = {
    rng =>
      val (a, r) = f(rng)
      g(a)(r)
  }

  def nonNegativeIntLessThan(n: Int): RNG[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if(i + (n-1) - mod > 0) unit(mod)
        else nonNegativeLessThan(n)
    }
  }


  def nonNegativeEvenInt: RNG[Int] = map(nonNegativeInt)(n => n - n % 2)

  def double_1: RNG[Double] = map(nonNegativeInt)(n => n + (n / (Integer.MAX_VALUE.toDouble + 1)))

  def intDouble_1: RNG[(Int, Double)] = both(nonNegativeInt, double)

  def doubleInt_1: RNG[(Double, Int)] = both(double, nonNegativeInt)
}