package datastructures

/**
 * Created by dmohan200 on 12/2/14.
 */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
   * Write a function size that counts the number of nodes (leaves and branches) in a tree.
   *
   * @param t Tree
   * @return number of nodes(branches + leaves) in the tree
   */
  def size[A](t: Tree[A]): Int = {
    def traverse[A](t: Tree[A], z: Int): Int = t match {
      case Leaf(_) => 1 + z
      case Branch(l, r) => 1 + z + traverse(l, 0) + traverse(r, 0)
    }
    traverse(t, 0)
  }

  /**
   *
   * @param t
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def map[A, B](t: Tree[A])(f: (Tree[A]) => Tree[B]): Tree[B] = t match {
    case Leaf(v) => f(Leaf(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
   *
   * @param t
   * @param z
   * @param c
   * @tparam A
   * @return
   */
  def max[A](t: Tree[A], z: A)(c: (A, A) => A): A = t match {
    case Branch(l, r) => max(l, max(r, z)(c))(c)
    case Leaf(v) => c(z, v)
  }

  /**
   *
   * @param t tree containing nodes of type A
   * @param a target node
   * @tparam A type of target node
   * @return
   */
  def depth[A](t: Tree[A], a: A): Int = {
    def traverse[A](t: Tree[A], depthAndFound: (Int, Boolean), res: List[(Int, Boolean)]): List[(Int, Boolean)] = t match {
      case Branch(l, r) => traverse(l, (depthAndFound._1 + 1, false), traverse(r, (depthAndFound._1 + 1, false), res))
      case Leaf(v) if(v == a) => Cons((depthAndFound._1 + 1, true), res)
      case Leaf(v) => Cons((0, false), res)
    }
    val res = traverse(t, (0, false), Nil:List[(Int, Boolean)])
    val depths = List.map(List.filter(res)(a => a._2))(a => a._1)

    List.max(depths, 0)(_ > _)
  }
}