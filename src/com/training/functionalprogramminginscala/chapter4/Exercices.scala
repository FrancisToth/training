package com.training.functionalprogramminginscala.chapter4

object Options extends App {

  /* Implement all of the preceding functions on Option. As you implement each function, try to think about what it
   means and in what situations you’d use it. We’ll explore when to use each of these functions next. Here are a
   few hints for solving this exercise:
     - It’s fine to use pattern matching, though you should be able to implement all the functions besides map and
       getOrElse without resorting to pattern matching.
     - For map and flatMap, the type signature should be enough to determine the implementation.
     - getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given
       default value.
     - orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
 */
  trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case Some(v) => Option(f(v))
      case _ => None
    }

    def getOrElse[B >: A] (default: => B): B = this match {
      case Some(v) => v
      case _ => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Option(_)).getOrElse(ob)
    def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
  }

  case class Some[A](a: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def apply[A](a: A): Option[A] = if(a == null) None else Some(a)

    /* Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is the mean of
   math.pow(x - m, 2) for each element x in the sequence. See the definition of variance on Wikipedia. */
    def mean(xs: Seq[Double]): Option[Double] = if(xs.isEmpty) None else Option(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

    /* Write a generic function map2 that combines two Option values using a binary function. If either Option value is
       None, then the return value is too. Here is its signature: */
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)) )

    /* Write a function sequence that combines a list of Options into one Option containing a list of all the Some values
       in the original list. If the original list contains None even once, the result of the function should be None;
       otherwise the result should be Some with a list of all the values. Here is its signature: */
    def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
      //    as.foldLeft(Option(List.empty[A])) { (result, optA) => map2(result, optA)(_ :+ _) }
      case Nil => Some(Nil)
      case x :: xs => map2(x, sequence(xs))(_ :: _)
    }

    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
      case Nil => Some(Nil)
      case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
    }

    def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(x => x)
  }
}

object Eithers extends App {
  /*Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value. */
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(v) => Right(f(v))
      case l:Left[E] => l
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(v) => f(v)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case _ => this
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(x => b.map(y => f(x,y)))
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    /* Implement sequence and traverse for Either. These should return the first error that’s encountered, if there
       is one. */
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
      case Nil => Right(Nil)
      case Left(e) :: xs => Left(e)
      case Right(x) :: xs => sequence(xs).map(l => x :: l)
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
    }

    def sequenceViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

    /* In this implementation, map2 is only able to report one error, even if both the name and the age are invalid.
       What would you need to change in order to report both errors? Would you change map2 or the signature of
       mkPerson? Or could you create a new data type that captures this requirement better than Either does, with some
       additional structure? How would orElse, traverse, and sequence behave differently for that data type ? */

    case class Person(name: Name, age: Age)
    sealed class Name(val value: String)
    sealed class Age(val value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty.") else Right(new Name(name))

    def mkAge(age: Int): Either[String, Age] = if (age < 0) Left("Age is out of range.") else Right(new Age(age))

    def mkPerson(name: String, age: Int): Either[String, Person] = mkName(name).map2(mkAge(age))(Person(_, _))

    trait Partial[+A,+B]
    case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
    case class Success[+B](get: B) extends Partial[Nothing,B]

//    def mkPerson2(name: String, age: Int): Partial[String, Person] =
//      mkName(name).map2(mkAge(age))(Person(_, _))
  }

  import com.training.functionalprogramminginscala.chapter4.Eithers.Either.sequence

  println(sequence(List(Right(1), Right(2))))
  println(sequence(List(Right(1), Left("what the !"))))
}
