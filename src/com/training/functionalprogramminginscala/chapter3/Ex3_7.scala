package com.training.functionalprogramminginscala.chapter3

import scala.annotation.tailrec


object Ex3_7 extends App {

  object List {

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
    def sum(ns: List[Double]) = foldRight(ns, 0.0)(_ + _)
    def length[A](as: List[A]): Int =  foldRight(as, 0)((_,b) => 1 + b)

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, (b:B) => b)((a, g) => b => g(f(b, a)))(z)
    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((b, a) => f(a, b))

    def appendRight[A](as: List[A], x: List[A]): List[A] = foldRight(as, x)(Cons(_, _))
    def appendLeft[A](as: List[A], x: List[A]): List[A] = foldLeft(as, (xs:List[A]) => xs) { (g, a) => tail => g(Cons(a, tail)) }(x)

    /* Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the
       total length of all lists. Try to use functions we have already defined. */
    def flatten[A](xs: List[List[A]]): List[A] = foldLeft(xs, Nil:List[A])(appendLeft)

    /* Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be
      a pure function that returns a new List!)*/
    def incrementEach(ints: List[Int]): List[Int] = foldRight(ints, Nil:List[Int])((a, b) => Cons(a + 1, b))

    /* Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString to
       convert some d: Double to a String.*/
    def doublesToString(doubles: List[Double]): String = foldRight(doubles, "")((a, b) => s"$a $b")

    /* Write a function map that generalizes modifying each element in a list while maintaining the structure of the
       list. */
    def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((a,b) => Cons(f(a), b))

    /* Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to
       remove all odd numbers from a List[Int]. */
    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((a,b) => if(f(a)) Cons(a, b) else b)

    /* Write a function flatMap that works like map except that the function given will return a list instead of a
       single result, and that list should be inserted into the final resulting list. For instance,
       flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3). */
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

    /* Use flatMap to implement filter. */
    def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if(f(a)) List(a) else Nil  )

    /* Write a function that accepts two lists and constructs a new list by adding corresponding elements. For example,
       List(1,2,3) and List(4,5,6) become List(5,7,9). */
    def addPairwise[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = (xs, ys) match {
      case (Cons(xh, xt), Cons(yh, yt)) => Cons(f(xh, yh), addPairwise(xt, yt)(f))
      case _ => Nil
    }

    /* Generalize the function you just wrote so that it’s not specific to integers or addition. Name your generalized
       function zipWith.*/
    def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
      case (Cons(xh, xt), Cons(yh, yt)) => Cons(f(xh, yh), zipWith(xt, yt)(f))
      case _ => Nil
    }

    /* Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a
       subsequence. For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among
       others. You may have some difficulty finding a concise purely functional implementation that is also efficient.
       That’s okay. Implement the function however comes most naturally. We’ll return to this implementation in
       chapter 5 and hopefully improve on it. Note: Any two values x and y can be compared for equality in Scala using
       the expression x == y. */

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      def iter(lefts: List[A], rights: List[A]): Boolean =
        (lefts, rights) match {
          case (_, Nil) => true
          case (Cons(a, as), Cons(b, bs)) => if(a == b) iter(as, bs) else iter(as, sub)
          case _ => false
        }
      iter(sup, sub)
    }

    // TREES

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    /* Write a function size that counts the number of nodes (leaves and branches) in a tree. */
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    /* Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala,
       you can use x.max(y) or x max y to compute the maximum of two integers x and y.)*/
    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

    /* Write a function depth that returns the maximum path length from the root of a tree to any leaf. */
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

    /* Write a function map, analogous to the method of the same name on List, that modifies each element in a tree
       with a given function. */
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    /* Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
       Reimplement them in terms of this more general function. Can you draw an analogy between this fold function and
       the left and right folds for List? */
    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
      case Leaf(a) => f(a)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

    def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _)
    def maximum2(tree: Tree[Int]): Int = fold(tree)(v => v)(_.max(_))
    def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l,r) => 1 + l.max(r))
    def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(v => Leaf(f(v)): Tree[B])((l,r) => Branch(l, r))
  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  import com.training.functionalprogramminginscala.chapter3.Ex3_7.List._

//  println(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
//  println(maximum(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))))
//  println(depth(Branch(Leaf(1), Branch(Leaf(3), Branch(Leaf(2), Leaf(4))))))
  println(map(Branch(Leaf(1), Branch(Leaf(3), Branch(Leaf(2), Leaf(4)))))(_ + 1))

//  println(hasSubsequence(List(1,2,3,4), List(1,2)))
//  println(hasSubsequence(List(1,2,3,4), List(2,3)))
//  println(hasSubsequence(List(1,2,3,4), List(4)))
//  println(hasSubsequence(List(1,2,3,4), Nil))
//  println(hasSubsequence(List(1,2,3,4), List(2,4)))
//  println(hasSubsequence(Nil, List(4)))

//  println(addPairwise(List(1,2,3), List(4,5,6))(_ + _))
//  println(addPairwise(List(1,2,3), List(4,5))(_ + _))
//  println(addPairwise(List(1,2), List(4,5,6))(_ + _))
//  println(length(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))))

//  println(foldLeft(List(1,2,3), 0)(_ + _))
//  println(sum(List(1.0,2.0,3.0)))
//  println(foldLeft2(List(1,2,3), 0)(_ + _))
//  println(foldRight2(List(1,2,3), 0)(_ + _))

//  println(foldLeft(List(1,2,3), 0)((a,b) => a - b))
//  println(foldLeft2(List(1,2,3), 0)((a,b) => a - b))

//  println(foldLeft(List(1,2,3), 0)((a,b) => a + b))
//  println(foldLeft2(List(1,2,3), 0)((a,b) => a + b))

//  println(foldLeft(List("1","2","3"), "")((a,b) => a + b))
//  println(foldLeft2(List("1","2","3"), "")((a,b) => a + b))

//  println(concatenate(List(List(1,2,3), List(4,5,6))))
//  println(flatMap(List(1,2,3))(a => List(a, a)))
//  println(filter2(List(1,2,3,4))(_ % 2 == 0))

//    println(incrementEach(List(1,2,3)))
//    println(doublesToString(List(1,2,3)))
//  println(map(List(1,2,3)) { a => a + 1 })
//  println(filter(List(1,2,3,4))(_ % 2 == 0))
//  println(appendRight(List("1","2","3"), List("4")))
//  println(appendLeft(List("1","2","3"), List("4")))
//  println(appendRight(List("1","2","3"), List("4")))

//  println(foldRight(List("1","2","3"), "")(_ + _))
//  println(foldRight2(List("1","2","3"), "")(_ + _))

//  println(foldLeft(List("1","2","3"),  Nil:List[String])((b,a) => Cons(b,a)))

//  println(foldRight(List("1","2","3"),  Nil:List[String])((a:String, b:List[String]) => Cons(a,b)))
//  println(foldRight2(List("1","2","3"),  Nil:List[String])((b,a) => Cons(a,b)))

//  println(foldLeft(List(1,2,3), 1)(_ * _))
//  println(product2(List(1.0,2.0,3.0)))
//  println(foldLeft2(List(1,2,3), 1)(_ * _))
//  println(foldRight2(List(1,2,3), 1)(_ * _))

//  println(foldLeft(List(1,2,3), 0)((b, _) => b + 1))
//  println(foldLeft(List(1,2,3), Nil:List[Int])((b, a) => Cons(a, b)))
}
