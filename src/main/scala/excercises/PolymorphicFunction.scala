package excercises

/**
 * Created by dmohan200 on 11/5/14.
 */
object PolymorphicFunction extends App {

  def findFirst[A](as: Array[A], eq: A => Boolean): Int = {

    def move(i: Int): Int = {
      if(i == as.length) -1
      else if (eq(as(i))) i
      else move(i+1)
    }
    move(0)
  }

  def isSorted[A](array: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def move(i: Int): Boolean = {
      if(i+1 >= array.length)
        true
      else if(ordered(array(i), array(i+1)))
        move(i + 1)
      else
        false
    }

    move(0)
  }

  println(findFirst(Array(1,2,3), (x: Int) => x == 2))
  println(findFirst(Array("abc", "efg", "hij"), (x: String) => x == "hij"))

  println(isSorted(Array(), (x: Int, y: Int) => x <= y))
  println(isSorted(Array(1,2,2,4,6), (x: Int, y: Int) => x <= y))
  println(isSorted(Array(1,2,5,4,6), (x: Int, y: Int) => x <= y))
  println(isSorted(Array(1,2,3,4,0), (x: Int, y: Int) => x <= y))

  println("doubles...")
  println(isSorted(Array(1.0,2.1,2.01,4.5,0.6), (x: Double, y: Double) => x <= y))
  println(isSorted(Array(1.0,2.1,2.1,4.5,6.6), (x: Double, y: Double) => x <= y))
  println(isSorted(Array(1.0,2.31,2.21,4.5,0.6), (x: Double, y: Double) => x <= y))

}
