package excercises

/**
 * Created by dmohan200 on 11/20/14.
 */
object PartialFunction {

  def partial[A, B, C](a: A, f: (A,B) => C ): B => C = {
    //(b: B) => f(a, b) // with type declaration
    b => f(a, b) //without type declaration
  }

  def currying[A, B, C](f: (A, B) => C): A => (B => C) = {

    //(a: A) => (b: B) => f(a, b)
    //a => b => f(a, b)

    def a_to_b2cfunc(a: A): B => C = {
      def b_to_cfunc(b: B): C = {
        f(a,b)
      }
      b_to_cfunc
    }
    a_to_b2cfunc
  }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    /*def ab_to_cfun(a: A, b: B): C = {
      f(a)(b)
    }
    ab_to_cfun*/

    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: A => B, g: B => C): A => C = {
    a => g(f(a))
  }

  def main(args: Array[String]) {
    println("Partial Functions...")
  }
}
