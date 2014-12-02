package datastructures

import java.util.NoSuchElementException

import scala.annotation.tailrec

/**
 * Created by dmohan200 on 11/20/14 12:18 AM.
 */
sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](h: A, tail: List[A]) extends List[A]

object List {

  /**
   * Implement the function tail for removing the first element of a List. Note that the
   * function takes constant time. What are different choices you could make in your
   * implementation if the List is Nil?
   *
   * @param as list of As
   * @tparam A type A
   * @return list with first element removed from l
   */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def head[A](as: List[A]): A = as match {
    case Nil => throw new NoSuchElementException
    case Cons(h, _) => h
  }

  /**
   * Using the same idea, implement the function setHead for replacing the first element
   * of a List with a different value.
   * @param l list of As
   * @param a a to set at head position
   * @tparam A type A
   * @return list l with head replaced with given a
   */
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Cons(a, Nil)
    case Cons(h, t) => Cons(a, t)
  }

  /**
   * Generalize tail to the function drop, which removes the first n elements from a list.
   * Note that this function takes time proportional only to the number of elements being dropped—
   * we don’t need to make a copy of the entire List.
   * @param l list of As
   * @param n number of elements to drop from beginning
   * @tparam A type A
   * @return
   */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if n>0 => drop(t, n-1)
    case Cons(h, t) => Cons(h, t)
  }

  /**
   * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
   * @param l list of As
   * @param f predicate to match
   * @tparam A type A
   * @return
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(tail(t))(f)
    case _ => l
  }

  /**
   * Adds all the elements of one list to the end of another
   * @param l1
   * @param l2
   * @tparam A
   * @return
   */
  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  /**
   * Implement a function, init, that returns a List consisting of all but the last element of a List.
   * So, given List(1,2,3,4), init will return List(1,2,3)
   *
   * @param l
   * @tparam A
   * @return
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
   *
   * @param list
   * @param init
   * @param add
   * @tparam A
   * @return
   */
  def sum[A](list: List[A], init: A, add: (A, A) => A): A = {
    list match {
      case Nil => init
      case Cons(h, t) => add(h, sum(t, init, add))
    }
  }

  /**
   *
   * @param list
   * @param init
   * @param multiply
   * @tparam A
   * @return
   */
  def product[A](list: List[A], init: A, multiply: (A, A) => A): A = {
    @tailrec
    def go(as: List[A], acc: A): A = {
      as match {
        case Nil => acc
        case Cons(h, t) => go(t, multiply(h, acc))
      }
    }
    go(list, init)
  }

  /**
   *
   * @param as list containing elements of type A
   * @param z accumulator with initial value
   * @param f function to apply on each elements in the list
   * @tparam A type A
   * @tparam B type B
   * @return result from applying the function f on each element in the given list
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  /**
   * Just a try. no improvement in performance
   * @param as list containing elements of type A
   * @param z accumulator with initial value
   * @param f function to apply on each elements in the list
   * @tparam A type A
   * @tparam B type B
   * @return result from applying the function f on each element in the given list
   */
  def foldRightTR[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    @tailrec
    def go(as: List[A], acc: B): B = {
      as match {
        case Nil => acc
        case Cons(h, t) => go(t, f(h, acc))
      }
    }
    go(as, z)
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
   *
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def fold[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => fold(t, f(h, z))(f)
  }

  /**
   * return the reverse of a list (given List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold.
   * @param as
   * @tparam A
   * @return
   */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))
  }

  def reverse2[A](as: List[A]): List[A] = {
    foldLeft(as, Nil:List[A])((z, a) => Cons(a, z))
  }

  def fill[A](a: A, n: Int): List[A] = {
    def go(l: List[A], i: Int): List[A] = {
      if(i < n) go(Cons(a, l), i + 1)
      else l
    }
    go(Cons(a, Nil), 1)
  }

  def length1[A](as: List[A]): Int = {
    @tailrec
    def go(as: List[A], z: Int): Int = as match {
      case Nil => z
      case Cons(_, t) =>  go(t, z+1)
    }
    go(as, 0)
  }

  def length2[A](as: List[A]): Int = {
    foldRight(as, 0)((_, z) => z+1)
  }

  def length[A](as: List[A]): Int = {
    foldLeft(as, 0)((z, a) => z + 1)
  }

  /**
   * reverse(as) => [ [7,8], [5,6], [4,3], [2,1]]
   * @param as
   * @tparam A
   * @return
   */
  def concatenate1[A](as: List[List[A]]): List[A] = {
    reverse(foldLeft(as, Nil: List[A])((z, a) => reverse(foldLeft(z, a)((z, a) => Cons(a, z)))))
  }

  def concatenate[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil:List[A])(append)
  }

  def max[A](as: List[A], init: A)(c:(A, A) => Boolean): A = {
    foldLeft(as, init)((z, a) => if(c(z, a)) z else a)
  }

  /**
   * transforms the given list by applying f on each element
   * @param as
   * @param f
   * @tparam A
   * @return
   */
  def map1[A, B](as: List[A])(f: A => B): List[B] = {
    foldLeft(as, Nil: List[B])((z, a) => Cons(f(a), z))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, Nil: List[A])((z, a) =>  if(f(a)) Cons(a, z) else z)
  }


  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concatenate(map(as)(f))
  }

  def zip[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zip(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(parent: List[A], child: List[A]): Boolean = (parent, child) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if(h1 == h2) {
          go(t1, t2)
        } else {
          go(t1, sub)
        }
    }
    go(sup, sub)
  }

  def take[A](as:List[A], n: Int): List[A] = {
    def go(as: List[A], i: Int, z: List[A]): List[A] = as match {
      case Nil => z
      case Cons(h, t) => if(i <= n) go(t, i+1, Cons(h, z)) else z
    }
    reverse(go(as, 1, Nil:List[A]))
  }

  /**
   * Constucts the singly linked list
   * @param as
   * @tparam A
   * @return
   */
  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

object Testing extends App {

  import List._
  //val l = Cons(1, Cons(2, Cons(3, Nil)))
  val ints = List(4, 5, 8, 1, 3, 2, 7, 6, 9)
  println(fill("deepak", 5))
  println(fill("buvana", 5))
  println(fill("kavinaya", 5))
  val intSum = sum(ints, 0, (a: Int, b: Int) => a + b)
  println(intSum)
  println("intSumTR: " + foldRightTR(ints, 0)((a, b) => a + b))
  println("intSum foldRightTailRecursive: " + foldRightTR(ints, 0)(_ + _))
  println("intProduct foldRightTailRecursive: " + foldRightTR(ints, 0)(_ * _))

  val ds = List(10.989d, 324.445d, 46.3434d)
  val doubleSum = sum(ds, 0.0, (a: Double, b: Double) => a + b)
  println("doubleSum:" + doubleSum)
  println("doubleProduct: " + List.product(ds, 1.0, (a: Double, b: Double) => a * b))

  println("list: " + ints)
  println(tail(ints))
  println(setHead(ints, 100))
  println(drop(ints, 2))
  println(dropWhile(ints)(x => x == 3))
  println(drop(ints, 4))
  println(append(ints, List(10, 20, 30, 40)))
  println(length(List.append(ints, List(10, 20, 30, 40))))
  println(init(ints))

  scala.List

}