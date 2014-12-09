package exceptionhandling

/**
 * Created by dmohan200 on 12/4/14 4:13 PM.
 */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](default: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => default
    case Right(_) => this
  }

  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(e) => Left(e)
    case Right(a) => b match {
      case Left(e) => Left(e)
      case Right(b) => Right(f(a, b))
    }
  }

  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(a => b.map(b => f(a, b)))
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      ar <- this
      br <- b
    } yield {
      f(ar, br)
    }
  }

  def map3[EE >: E, B, C, D](b: Either[EE, B], c: Either[EE, C])(f: (A, B, C) => D): Either[EE, D] = {
    for {
      ar <- this
      br <- b
      cr <- c
    } yield {
      f(ar, br, cr)
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] = {
    try
      Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def mean[A](as: List[Double]): Either[String, Double] = {
    if(as.isEmpty)
      Left("mean of empty list")
    else
      Right(as.sum/as.length)
  }

  def sequence[E, A](eas: List[Either[E, A]]): Either[E, List[A]] = eas match {
    case Nil => Right(List[A]())
    case ea :: easTail => ea flatMap(a => sequence(easTail) map(a :: _))
  }

  def traverse[E, A, B](eas: List[Either[E, A]])(f: A => B): Either[E, List[B]] = eas match {
    case Nil => Right(List[B]())
    case ea :: eaTail => ea flatMap(a => traverse(eaTail)(f) map (bs => f(a) :: bs))
  }
}
