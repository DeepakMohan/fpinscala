package exceptionhandling

import scala.annotation.tailrec

/**
 * Created by dmohan200 on 12/3/14 8:45 AM.
 */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(v) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(as: Seq[Double]): Option[Double] = if(!as.isEmpty) Some(as.sum / as.length) else None

  /**
   * Implement the variance function in terms of flatMap. If the mean of a sequence is m,
   * the variance is the mean of math.pow(x - m, 2) for each element x in the sequence
   *
   * @param as
   * @return
   */
  def variance1(as: Seq[Double]): Option[Double] = {
    mean(as) map {m =>
      mean(as.map(x => Math.pow(x-m, 2))).getOrElse(0D)
    }
  }

  def variance(as: Seq[Double]): Option[Double] = {
    mean(as).flatMap(m => mean(as.map(x => Math.pow(x-m, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  /**
   * Write a generic function map2 that combines two Option values using a binary function.
   * If either Option value is None, then the return value is too.
   */
  def map2_1[A, B, C](as: Option[A], bs: Option[B])(f: (A, B) => C): Option[C] = (as, bs) match {
    case (Some(_), None) => None
    case (None, Some(_)) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  /**
   * Write a generic function map2 that combines two Option values using a binary function.
   * If either Option value is None, then the return value is too.
   */
  def map2_2[A, B, C](as: Option[A], bs: Option[B])(f: (A, B) => C): Option[C] = {
    as flatMap(a => bs map(b => f(a, b)))
  }

  /**
   * Write a generic function map2 that combines two Option values using a binary function.
   * If either Option value is None, then the return value is too.
   */
  def map2[A, B, C](as: Option[A], bs: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      a <- as
      b <- bs
    } yield {
      f(a, b)
    }
  }

  /**
   * flatMap and map combination
   * @return
   */
  def map3_1[A, B, C, D](as: Option[A], bs: Option[B], cs: Option[C])(f: (A, B, C) => D): Option[D] = {
    as flatMap(a => bs flatMap(b => cs map(c => f(a, b, c))))
  }

  /**
   * rewrite map3_1 using for comprehension
   * @return
   */
  def map3[A, B, C, D](as: Option[A], bs: Option[B], cs: Option[C])(f: (A, B, C) => D): Option[D] = {
    for {
      a <- as
      b <- bs
      c <- cs
    } yield {
      f(a, b, c)
    }
  }

  /**
   * EXERCISE 4.4: Write a function sequence that combines a list of Options into one Option containing
   * a list of all the Some values in the original list. If the original list contains None even
   * once, the result of the function should be None; otherwise the result should be Some
   * with a list of all the values
   *
   * @param opts
   * @tparam A
   * @return None or Some containing list of values
   */
  def sequence1[A](opts: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(ops: List[Option[A]], z: Option[List[A]]): Option[List[A]] = ops match {
      case Nil => z
      case x :: xs =>
        x match {
          case None => None
          case Some(v) => z match {
            case None => None
            case Some(zs) => go(xs, Some(zs :+ v))
          }
        }
    }
    go(opts, Some(List[A]()))
  }

  def sequence2[A](opts: List[Option[A]]): Option[List[A]] = {
    def go(ops: List[Option[A]], z: Option[List[A]]): Option[List[A]] = ops match {
      case Nil => z
      case x :: xs => x flatMap(xv => z flatMap(zv => go(xs, Some(zv :+ xv))))
    }

    go(opts, Some(List[A]()))
  }

  /**
   * EXERCISE 4.4: Write a function sequence that combines a list of Options into one Option containing
   * a list of all the Some values in the original list. If the original list contains None even
   * once, the result of the function should be None; otherwise the result should be Some
   * with a list of all the values
   *
   * differ from above impl is traverse through entire list list even if encounters None before keeping z value None
   * @param opts
   * @tparam A
   * @return
   */
  def sequence3[A](opts: List[Option[A]]): Option[List[A]] = {
    def go(ops: List[Option[A]], z: Option[List[A]]): Option[List[A]] = ops match {
      case Nil => z
      case x :: xs => go(xs, map2(x, z)((xv, zv) => zv :+ xv))
    }
    go(opts, Some(List[A]()))
  }

  def sequence[A](opts: List[Option[A]]): Option[List[A]] = opts match {
    case Nil => Some(Nil)
    case opt :: optTail => opt flatMap(o => sequence(optTail) map (o :: _))
  }

  def Try[A](a: => A): Option[A] = {
    try
      Some(a)
    catch {
      case e: Exception => None
    }
  }

  def parseInts(a: List[String]): Option[List[Int]] = sequence2(a map (i => Try(i.toInt)))

  /**
   * brutal way
   *
   * @param as
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def traverse0[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(as map(f))
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    def go(as: List[A], z:Option[List[B]]): Option[List[B]] = as match {
      case Nil => z
      case a :: as => go(as, map2(f(a), z)((ar, zv) => zv :+ ar))
    }
    go(as, Some(List[B]()))
  }

  def parseInts_1(a: List[String]): Option[List[Int]] = traverse(a)(i => Try(i.toInt))
}