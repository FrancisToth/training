package com.training.functionalprogramminginscala

import scala.annotation.tailrec

object chapter5 extends App {

  sealed trait Stream[+A] {
    import chapter5.Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    /* Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the
     REPL. You can convert to the regular List type in the standard library. You can place this and other functions
     that operate on a Stream inside the Stream trait.*/
    def toList: List[A] = this match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }

    def toList2: List[A] = {
      @tailrec
      def iter(acc: List[A], stream: Stream[A]): List[A] = stream match {
        case Cons(h, t) => iter(h() :: acc, t())
        case _ => acc
      }
      iter(List.empty[A], this).reverse
    }

    /* Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first
       n elements of a Stream.*/
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    /* Write the function takeWhile for returning all starting elements of a Stream that match the given predicate. */
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)(p(_) || _)

    /* Implement forAll, which checks that all elements in the Stream match a given predicate. Your implementation
       should terminate the traversal as soon as it encounters a non matching value. */
    def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

    /* Use foldRight to implement takeWhile. */
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (h, t) => if(p(h)) cons(h, t) else t }

    /* Hard: Implement headOption using foldRight. */
    def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A]) { (h, _) => Option(h) }

    /* Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its
       argument. */
    def map[B](f: A => B): Stream[B] = foldRight(empty[B]) { (h, t) => Cons[B](() => f(h), () => t) }

    def filter(p: A => Boolean) = foldRight(empty[A]) { (h, t) => if(p(h)) cons(h, t) else t }

    def append[B >: A](a: => Stream[B]): Stream[B] = foldRight(a) { (h, t) => cons(h, t) }

    def flatMap[B](f: A => Stream[B]) = foldRight(empty[B]) { (h, t) => f(h).append(t) }

    /* Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll. The zipAll function should
       continue the traversal as long as either stream has more elements—it uses Option to indicate whether each stream
       has been exhausted.*/
    def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Option(f(h()), t())
      case _ => None
    }

    def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, t), i) if i > 0 => Option(h(), (t(), i - 1))
      case _ => None
    }

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if p(h()) => Option(h(), t())
      case _ => None
    }

    def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Option(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

    def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s)) {
      case (Cons(h, t), Cons(h1, t1)) => Option((Option(h()), Option(h1())), (t(), t1()))
      case (Cons(h, t), Empty) => Option((Option(h()), None), (t(), empty))
      case (Empty, Cons(h1, t1)) => Option((None, Option(h1())), (empty, t1()))
      case _ => None
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

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    /* Generalize ones slightly to the function constant, which returns an infinite Stream of a given value. */
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    /* Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on. */
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    /* Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on. */
    def fibs: Stream[Int] = {
      def iter(n: Int = 0, m: Int = 1): Stream[Int] = cons(n, iter(m, m + n))
      iter()
    }

    /* Write a more general stream-building function called unfold. It takes an initial state, and a function for
       producing both the next state and the next value in the generated stream. */
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

    /* Write fibs, from, constant, and ones in terms of unfold. */
    def fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Option((a, (b, a + b))) }
    def fromViaUnfold(n: Int): Stream[Int] = unfold(n) { (a) => Option((a, a + 1)) }
    def constantViaUnfold[A](a: A): Stream[A] = unfold(a)( a => Option(a, a))
    def ones: Stream[Int] = constantViaUnfold(1)

    /* Hard: Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
       For instance, Stream(1,2,3) startsWith Stream(1,2) would be true. */
//    def startsWith[A](s: Stream[A]): Boolean

    /* Implement tails using unfold. For a given Stream, tails returns the Stream of suf- fixes of the input sequence,
       starting with the original Stream. For example, given Stream(1,2,3), it would return
       Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).*/
//    def tails: Stream[Stream[A]]

    /* Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the
       intermediate results. For example:

        scala> Stream(1,2,3).scanRight(0)(_ + _).toList
        res0: List[Int] = List(6,5,3,0)

       This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0). Your function should reuse
       intermediate results so that traversing a Stream with n elements always takes time linear in n. Can it be
       implemented using unfold? How, or why not? Could it be implemented using another function we’ve written? */
  }

  val s = Stream(1,2,3,4,5,6)
  val s2 = Stream("a", "b", "c", "d", "e", "f")

  println(s.zipWith(s2)((i, c) => s"$i $c").toList)
  println(s.take(3).zipAll(s2).toList)
//  println(s.take(2).toList)
//  println(s.drop(2).toList)
//  println(s.takeWhile(_ < 4).toList)
//  println(s.forAll(_ < 3))
//  println(Stream.empty[Int].headOptionViaFoldRight)
//  println(s.filter(_ > 1).toList)
//  println(s.flatMap( i => Stream(i+1)).toList)
//  println(Stream.fibsViaUnfold.take(7).toList)
//  println(Stream.fromViaUnfold(0).take(5).toList)
//  println(Stream.constantViaUnfold(2).take(5).toList)
//  println(Stream.ones.take(5).toList)

}
