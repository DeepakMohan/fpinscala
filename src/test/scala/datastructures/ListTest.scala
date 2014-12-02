package datastructures

import org.scalatest.{BeforeAndAfter, FunSuite}

/**
 * Created by dmohan200 on 11/28/14 12:16 AM.
 */
class ListTest extends FunSuite with BeforeAndAfter {

  import List._

  test("fill") {
    assert(List("deepak", "deepak", "deepak", "deepak") == fill("deepak", 4))
    assert(List("buvana", "deepak", "kavinaya") != fill("buvana", 3))
  }

  test("sum") {
    assert(10 == sum(List(1,2,3,4), 0, (a: Int, b: Int) => a + b))
  }

  test("product") {
    assert(24 == product(List(1,2,3,4), 1, (a: Int, b: Int) => a * b))
  }

  test("length") {
    assert(4 == length(fill("a", 4)))
  }

  test("foldRightTailRecursive") {
    val ints = List(1,2,3)
    assert(ints ==
      foldRight(ints, Nil: List[Int]  ){ (elem, accum) => Cons(elem, accum)}
    )
    assert(List(3, 2, 1) ==
      foldRightTR(ints, Nil: List[Int]){ (elem: Int, accum: List[Int]) => Cons(elem, accum)  }
    )

    assert(6 == foldRightTR(ints, 0)(_ + _))
  }

  test("foldLeft") {
    val l = List(1, 2, 3, 4, 5)
    assert(15 == foldLeft(l, 0)(_ + _), "sum must be 10")
    assert(120 == foldLeft(l, 1)(_ * _), "product must be 24")
    assert(5 == foldLeft(l, 0) ((b, a) => b + 1), "length must be 5")
  }

  test("reverse List") {
    val l = List(1, 2, 3, 4, 5)
    assert(List(5, 4, 3, 2, 1) == foldLeft(l, Nil: List[Int])((b, a) => Cons(a, b)))
    assert(List(5, 4, 3, 2, 1) == reverse(l))
  }

  //EXERCISE 3.14
  test("append using foldRight") {
    val l1 = List(1, 2, 3, 4, 5)
    val l2 = List(6, 7, 8, 9, 10)
    assert(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) ==
      foldRight(l1, l2)((a, b) => Cons(a, b))
    )
  }

  //EXERCISE 3.14
  test("append using foldLeft") {
    val l1 = List(1, 2, 3, 4, 5)
    val l2 = List(6, 7, 8, 9, 10)
    assert(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) ==
      foldLeft(reverse(l1), l2)((z, a) => Cons(a, z))
    )
  }

  test("concatenate") {
    assert(List(1,2,3,4,5,6,7,8) == concatenate(List(List(1, 2), List(3, 4), List(5, 6), List(7, 8))))
    assert(8 == length(concatenate(List(List(1, 2), List(3, 4), List(5, 6), List(7, 8)))))
  }

  test("EXERCISE 3.16: transform") {
    val l = List(1, 2, 3, 4)
    val transformed = map(l)(_ + 1)
    assert(List(1, 2, 3, 4) == l) // l retains its state
    assert(List(2, 3, 4, 5) == transformed)
  }

  test("EXERCISE 3.17: transform") {
    assert(List("1.0", "2.0", "3.0", "4.0", "5.0") == map(List(1d, 2d, 3d, 4d, 5d))((d: Double) => d.toString))
  }

  test("EXERCISE 3.19: filter") {
    assert(List(2, 4, 6, 8, 10) == reverse(filter(List(1,2,3,4,5,6,7,8,9,10))(_ % 2 == 0)))
  }

  test("EXERCISE 3.20: flatMap") {
    assert(List(1,1,2,2,3,3) == flatMap(List(1,2,3))(i => {List(i,i)}))
  }

  test("EXERCISE 3.21: flatMap filter") {
    assert(List(2,2,4,4,6,6) == flatMap(List(1,2,3,4,5,6,7))(i => if(i%2 == 0) List(i,i) else List()))
  }

  test("Exercise 3.22: apply functions on parallel elements") {
    assert(List(5, 7, 9) == zip(List(1,2,3), List(4,5,6))(_ + _))
    assert(List("deepak", "buvana", "kavinaya") == zip(List("dee", "buv", "kavi"), List("pak", "ana", "naya"))(_ concat _))
  }

  test("max") {
    assert(11 == max(List(1,11,2,4,3,4), 0)(_ > _))
  }

  test("take n") {
    assert(List(1,2,3,4) == take(List(1,2,3,4,5,6,7,8,9,0), 4))
  }

  test("EXERCISE 3.24: hasSubsequence") {
    assert(hasSubsequence(List(1,2,3,4,5,6,7,8), List(4,5,6)))
    assert(!hasSubsequence(List(1,2,3,4,5,6,7,8), List(4,5,7)))
  }
}
