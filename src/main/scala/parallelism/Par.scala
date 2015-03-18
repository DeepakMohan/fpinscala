package parallelism

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

/**
 * Created by dmohan200 on 3/3/15 11:00 PM.
 */
object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A): Par[A] = {
    (e: ExecutorService) => UnitFuture(a)
  }

  private case class UnitFuture[A](a: A) extends Future[A] {
    def isDone: Boolean = true
    def get: A = a
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    def isCancelled: Boolean = false
    def get(timeout: Long, unit: TimeUnit): A = get
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
    a(s)
  }

  /**
   * The given method f is not evaluated in a separate logical thread.
   */
  /**
   *
   * EX 7.3 TODO: Fix the implementation of map2 so that it respects the contract of timeouts on Future.
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val futureA: Future[A] = a(es) //evaluates A in parallel
      val futureB: Future[B] = b(es) //evaluates B in parallel
      UnitFuture(f(futureA.get, futureB.get)) //wait for A and B and then evaluate f(A,B) to get C
    }
  }

  def map2_withTimeout[A, B, C](a: Par[A], b: Par[B], timeout: Long)(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val futureA: Future[A] = a(es) //evaluates A in parallel
      val futureB: Future[B] = b(es) //evaluates B in parallel

      val start = System.currentTimeMillis
      val aVal: A = futureA.get(timeout, TimeUnit.MILLISECONDS)
      val remains = timeout - (System.currentTimeMillis - start)
      val bVal:B = futureB.get(remains, TimeUnit.MILLISECONDS)
      UnitFuture(f(aVal, bVal))
    }
  }

  /**
   *
   */
  def fork[A](a: => Par[A]): Par[A] = {
    (es: ExecutorService) => es.submit(new Callable[A] {
      def call(): A = a(es) get // call() will wait till the inner task - a(es) - to complete
    })
  }

  def lazyUnit[A](a: A): Par[A] = {
    fork(unit(a))
  }

  /**
   * EXERCISE 7.4: using lazyUnit, write a function to convert any function A => B to one that evaluates its result asynchronously.
   *
   * A => B  to A => Par[B]
   */
  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def sortPar(asPar: Par[List[Int]]): Par[List[Int]] = {
    map2(asPar, unit(()))((a, _) => a.sorted)
  }

  def map[A, B](aPar: Par[A])(f: A => B): Par[B] = {
    map2(aPar, unit(()))((a, _) => f(a))
  }

  def sortParUsingMap(asPar: Par[List[Int]]): Par[List[Int]] = {
    //map(asPar)(as => as.sorted)
    map(asPar)(_.sorted)
  }

  /**
   *
   * @param as list of As
   * @param f function that take A and gives B
   * @return an executable which results list of Bs
   */
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
    val bs: List[Par[B]] = as map asyncF(f)
    sequence(bs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    as.foldLeft(unit(List.empty[A]))((acc, a) => map2(acc, unit(a))((as, a) => if(f(a)) as :+ a else as))
  }

  def sequence[A](executables: List[Par[A]]): Par[List[A]] = {
    executables.foldLeft(unit(List.empty[A]))((acc, aPar) => map2(acc, aPar)((as, a) => as :+ a))
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    ???
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] = {
    ???
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A,B,C,D) => E): Par[E] = {
    ???
  }

  def wordCount(paragraphs: List[String]): Par[Int] = {
    ???
  }
}