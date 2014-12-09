package datastructures

import org.scalatest.FunSuite

/**
 * Created by dmohan200 on 12/2/14.
 */
class TreeTest extends FunSuite {

  import Tree._

  val t = Branch[Int](Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  test("EXERCISE 3.25: count") {
    assert(7 == size(Branch[Int](Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
  }

  test("EXERCISE 3.26: maximum") {
    assert(4 == max(t, 0)(_ max _))

    //
    val strs = Branch[String](Branch(Leaf("abcd"), Leaf("abcdefgh")), Branch(Leaf("abc"), Leaf("efghi")))
    assert("abcdefgh" == max(strs, "")((a, b) => if(a.length >= b.length) a else b))

    assert("" == max(Leaf(""), "")((a, b) => if(a.length >= b.length) a else b))
  }

  test("EXERCISE 3.27: distance") {
    val t = Branch[Int](Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
    assert(3 == depth(t, 2))
    assert(0 == depth(t, 10))
  }


  test("Exercise 3.28: map") {
    val transformed = map(t) {
      node => node match { //swapping branch nodes and double leaf value
        case Leaf(v) => Leaf(v + v)
        case Branch(l, r) => Branch(r, l)
      }
    }
    assert(Branch[Int](Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8))) == transformed)
  }

}
