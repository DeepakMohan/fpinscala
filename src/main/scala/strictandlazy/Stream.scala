package strictandlazy

/**
 * Created by dmohan200 on 12/5/14 4:24 PM.
 */
sealed trait Stream[+A] {

  import Stream._

  def headOption_1: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((h, _) => if(h == None) None else Some(h))
  }

  def toList: List[A] = this match {
    case Empty => List[A]()
    case Cons(h, t) => h() :: t().toList
  }

  /**
   * Write the function take(n) for returning the first n elements of a Stream
   *
   * @param n
   * @return
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def take_1(n: Int): Stream[A] = {
    def go(s: Stream[A], i: Int): Stream[A] = s match {
      case Cons(h, t) if i < n => cons(h(), go(t(), i + 1))
      case _ => Empty
    }
    go(this, 0)
  }

  def take_unfold(n: Int): Stream[A] = {
    unfold((this, n))(z => z match {
      case (Cons(h, t), nn) if nn > 0 => Some((t(), nn-1), h())
      case _ => None
    })
  }

  /**
   * Skipping the first n elements of a Stream.
   *
   * @param n
   * @return
   */
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) => Cons(h, t)
  }

  /**
   * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
   * @param p predicate to match
   * @return stream containing As which satisfies given predicate p
   *
   * h() - will be executed only once
   */
  def takeWhile_1(p: A => Boolean): Stream[A] = {
    def go(s: Stream[A], z: Stream[A]): Stream[A] = s match {
      case Empty => z
      case Cons(h, t) if p(h()) => go(t(), cons(h(), z))
      case Cons(h, t) => z
    }
    go(this, Empty)
  }

  /**
   * not stack safe
   *
   * @param p
   * @return
   */
  def takeWhile_2(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile_2(p))
    case _ => Empty
  }

  /**
   * takeWhile using foldRight
   * @param p predicate to match
   * @return
   */
  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if(p(a)) cons(a, b) else Empty)
  }

  def takeWhile_unfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(t(), h())
      case _ => None
    }
  }

  def exists_1(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
   *
   * @param p
   * @return
   */
  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def map[B](z: Stream[B])(f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, z) => cons(f(a), z))
  }

  def map_unfold[B](z: Stream[B])(f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(t(), f(h()))
      case Empty => None
    }
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, z) => if(f(a)) cons(a, z) else z)
  }

  def append[AA >: A](to: => Stream[AA]): Stream[AA] = {
    foldRight(to)((a, z) => cons(a, z))
  }

  def flatMap[B](z: Stream[B])(f: A => Stream[B]): Stream[B] = {
    //foldRight(Empty: Stream[B])((a, z) => f(a).map(z)(b => b).append(z))
    foldRight(Empty: Stream[B])((a, z) => f(a).append(z))
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, bs)) {
      case (Empty, _) | (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((t1(), t2()), f(h1(), h2()))
    }
  }

  /**
   *
   * @param bs stream may contain lesser or more elements than this stream
   * @tparam B type of elements in streat bs
   * @return
   */
  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {

    def corec[A, B](z: (Stream[A], Stream[B])): Option[ ((Stream[A], Stream[B]), (Option[A], Option[B])) ] =
      z match {
        case (Empty, Empty) => None
        case (Cons(h1, t1), Empty) => Some((t1(), Empty: Stream[B]), (Some(h1()), None: Option[B]))
        case (Empty, Cons(h2, t2)) => Some((Empty: Stream[A], t2()), (None: Option[A], Some(h2())))
        case (Cons(h1, t1), Cons(h2, t2)) => Some((t1(), t2()), (Some(h1()), Some(h2())))
      }

    unfold((this, bs))(z => corec(z))
  }

  /**
   * infinite streams may cause stack overflow for un-matching sub sequence.
   * ???
   * @param sub
   * @tparam B
   * @return
   */
  def hasSubsequence[B](sub: Stream[B]): Boolean = (this, sub) match {
    case (Empty, Empty) | (Empty, _) => false
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) => if (h1() == h2()) t1().hasSubsequence(t2()) else t1().hasSubsequence(sub)
  }

  def hasSubsequence_1[B](sub: Stream[B]): Boolean = {
    this.tails.exists(_ startsWith(sub))
  }
  /**
   * Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
   * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
   *
   * the below function composition is interleaved and so, there will not be any intermediate list
   *
   * @param as
   * @tparam AA
   * @return
   */
  def startsWith[AA >: A](as: Stream[AA]): Boolean = {
    zipAll(as).takeWhile(pair => pair._2 != None).forAll(pair => pair._1 == pair._2)
  }

  /**
   * Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes
   * of the input sequence, starting with the original Stream.
   * For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
   * @return
   */
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some((t(), cons(h(), t())))
      case _ => None
    }
  }

  /**
   *
   * @param z initial state and intermediate state or accumulator
   * @param f
   * @tparam B
   * @return
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    // 'intermediate' is a pair of intermediate state and intermediate stream maintained for re-usability
    foldRight( (z, Stream(z)) ) ( (a, intermediate) => {
      lazy val intermediateRes = f(a, intermediate._1) //intermediate._1 is z
      (intermediateRes, cons(intermediateRes, intermediate._2))
    })._2
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  //infinte streams
  def ones: Stream[Int] = {
    lazy val oness = cons(1, ones)
    oness
  }

  def ones_unfold: Stream[Int] = {
    unfold(1)(one => Some((1,1)))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val const: Stream[A] = Cons(() => a, () => const)
    //lazy val const2: Stream[A] = Stream.cons(a, const2) //every time allocates stream containing 'a'
    const
  }

  def constant_unfold[A](a: A): Stream[A] = {
    unfold(a)(z => Some((z, z)))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def from_unfold(n: Int): Stream[Int] = {
    unfold(n)(z => Some((z+1, z)))
  }

  def fibonacci: Stream[Int] = {
    def go(s1: Int, s2: Int): Stream[Int] = {
      lazy val series = cons(s1, go(s2, s1+s2))
      series
    }
    go(0,1)
  }

  def fibonacci_unfold: Stream[Int] = {
    unfold((0,1))(z => Some((z._2, z._1 + z._2), z._1))
  }

  /**
   * A stream-building function called unfold. It takes an initial state,
   * and a function for producing both the next state and the next value
   * in the generated stream.
   *
   * @param z initial state
   * @param f function to produce next state and next value (lazily evaluated)
   * @tparam A value type
   * @tparam S type of initial and next state
   * @return stream containing initial state and lazily evaluable next state
   */
  def unfold[A, S](z: S)(f: S => Option[(S, A)]): Stream[A] = {
    f(z) match {
      case Some((s, a)) => cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }


}