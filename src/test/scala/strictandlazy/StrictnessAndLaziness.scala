package strictandlazy

import org.scalatest.FunSuite

/**
 * Created by dmohan200 on 12/5/14 2:42 PM.
 */
class StrictnessAndLaziness extends FunSuite {

  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = {
    if(cond) onTrue() else onFalse()
  }

  test("if2") {
    val x = 30
    if2((x > 10), {() => println("a")}, {() => println("b")})
    if2((x < 10), {() => println("a")}, {() => println("b")})
  }

  test("lazy") {
    /*lazy val now = System.currentTimeMillis()
    println("1. before wait " + System.currentTimeMillis())
    Thread.sleep(1000)
    val now1 = now
    Thread.sleep(1000)
    val now2 = now
    assert(now1 == now2)*/
  }

  test("headOption") {
    val s: Stream[Int] = Stream(1,2,3)
    assert(Some(1) == s.headOption)

    assert(None == Stream(None, 12, 10).headOption)
    assert(Some(12) == Stream(12, None, 10).headOption)
  }

  test("EXERCISE 5.1: toList") {
    val stream: Stream[Int] = Stream("scala".length, "java".length, "oops".length, "fp".length)
    assert(List(5, 4, 4, 2) == stream.toList)
    assert(List(1,2,3,4,5) == Stream(1,2,3,4,5).toList)
  }

  test("EXERCISE 5.2a: take(n)") {
    assert(Stream(1, 2, 3).toList == Stream(1,2,3,4,5,6).take(3).toList)

    assert(Stream(1, 2, 3).toList == Stream(1,2,3,4,5,6).take_unfold(3).toList)
  }

  test("EXERCISE 5.2b: drop(n)") {
    assert(Stream(4, 5, 6).toList == Stream(1,2,3,4,5,6).drop(3).toList)
  }

  test("EXERCISE 5.3: takeWhile") {
    assert(Stream(10, 20, 30, 40).toList == Stream(10, 20, 30, 40, 45, 50, 60).takeWhile(_ % 10 == 0).toList)

    assert(Stream(10, 20, 30, 40).toList == Stream(10, 20, 30, 40, 45, 50, 60).takeWhile_unfold(_ % 10 == 0).toList)
  }

  test("EXERCISE 5.3: forAll") {
    assert(Stream(10, 20, 30, 40, 50, 60).forAll(_ % 10 == 0))
    assert(!Stream(1, 10, 20, 30, 40, 50, 60).forAll(_ % 10 == 0))
    assert(!Stream(10, 20, 30, 55, 40, 50, 60).forAll(_ % 10 == 0))
    assert(!Stream(10, 20, 30, 40, 50, 60, 65).forAll(_ % 10 == 0))
  }

  test("EXERCISE 5.7: map") {
    assert(Stream(4, 9, 16, 25, 36).toList == Stream(2,3,4,5,6).map(Empty: Stream[Int])(a => a*a).toList)
    assert(Stream('s', 'j', 'o', 'f').toList == Stream("scala", "java", "oops", "fp").map(Empty: Stream[Char])(a => a.charAt(0)).toList)

    //unfold
    assert(Stream(4, 9, 16, 25, 36).toList == Stream(2,3,4,5,6).map_unfold(Empty: Stream[Int])(a => a*a).toList)
  }

  test("EXERCISE 5.7: filter") {
    assert(Stream(10, 20, 30, 40).toList == Stream(10, 15, 20, 25, 30, 35, 40, 45).filter(_ % 10 == 0).toList)
    assert(Stream("scala", "java", "oops", "fp").toList == Stream("scala", "java", "oops", "fp","ruby", "groovy").filter(a => List('s', 'j', 'o', 'f').contains(a.charAt(0))).toList)
  }

  test("EXERCISE 5.7: append") {
    assert(Stream(1,2,3).toList == Stream().append(Stream(1,2,3)).toList)
    assert(Stream(1,2,3).toList == Empty.append(Stream(1,2,3)).toList)
    assert(Stream(1,2,3, 4,5,6).toList == Stream(1,2,3).append(Stream(4,5,6)).toList)
  }

  test("EXERCISE 5.7: flatMap") {
    assert(Stream(2,2,4,4,6,6).toList == Stream(1,2,3,4,5,6,7).flatMap(Empty: Stream[Int])(a => if(a%2==0)Stream(a, a) else Empty).toList)
  }

  //infinite streams
  test("EXERCISE 5.8 constant") {
    assert(Stream("deepak", "deepak", "deepak").toList == Stream.constant("deepak").take(3).toList)
    assert(Stream.constant("scala").exists(_ == "scala"))

    //constant_unfold
    assert(Stream("deepak", "deepak", "deepak").toList == Stream.constant_unfold("deepak").take(3).toList)
    assert(Stream.constant_unfold("scala").exists(_ == "scala"))
  }

  test("EXERCISE 5.9 from - infinite series") {
    Stream.from(10)
//    assert(Stream(10,11,12,13,14).toList == Stream.from(10).take(5).toList)
//    assert(Stream(10,11,12,13,14).toList == Stream.from_unfold(10).take(5).toList)
  }

  test("EXERCISE 5.10: fibonacci") {
    assert(Stream(0,1,1,2,3,5,8,13).toList == Stream.fibonacci.take(8).toList)
    assert(Stream(0,1,1,2,3,5,8,13).toList == Stream.fibonacci_unfold.take(8).toList)
  }

  test("EXERCISE 5.13: zipWith") {
    assert(Stream("fp in scala", "fp in java8").toList == Stream("fp in ", "fp in ").zipWith(Stream("scala", "java8"))(_ + _).toList)
  }

  test("EXERCISE 5.13: zipAll") {
    assert(Stream((Some(1), Some("a")), (Some(2), Some("b"))).toList == Stream(1,2).zipAll(Stream("a", "b")).toList)
    assert(Stream((Some(1), Some("a")), (Some(2), None)).toList == Stream(1,2).zipAll(Stream("a")).toList)
    assert(Stream((Some(1), Some("a")), (None, Some("b"))).toList == Stream(1).zipAll(Stream("a", "b")).toList)
  }

  test("hasSubsequence: ") {
    assert(Stream(30, 40).hasSubsequence(Stream(30)))
    assert(!Stream("s", "c", "a", "la").hasSubsequence(Stream("c", "a", "la")))
    assert(Stream("fp", "in", "s", "c", "a", "la").hasSubsequence(Stream("s", "c", "a")))
  }

  test("hasSubsequence: using tails ") {
    assert(Stream(30, 40).hasSubsequence_1(Stream(30)))
    assert(Stream("s", "c", "a", "la").hasSubsequence_1(Stream("c", "a", "la")))
    assert(Stream("fp", "in", "s", "c", "a", "la").hasSubsequence_1(Stream("s", "c", "a")))
  }

  test("EXERCISE 5.14: startsWith") {
    assert(Stream("s", "c", "a", "la").startsWith(Stream("s", "c", "a")))
    assert(Stream("s", "c", "a", "la").startsWith(Stream("s", "c")))
    assert(Stream("s", "c", "a", "la").startsWith(Stream("s")))
    assert(Stream("s", "c", "a", "la").startsWith(Stream()))
  }

  test("EXERCISE 5.15 tails") {
    println(Stream(1,2,3).tails.toList)
    //assert(Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).toList == Stream(1,2,3).tails.toList)
  }

  test("EXERCISE 5.16 scanRight") {
    assert(Stream(6,5,3,0).toList == Stream(1,2,3).scanRight(0)(_ + _).toList)

    assert(Stream("deepak", "eepak", "epak", "pak", "ak", "k", "").toList == Stream("d","e","e","p","a","k").scanRight("")(_ + _).toList)
  }

}
