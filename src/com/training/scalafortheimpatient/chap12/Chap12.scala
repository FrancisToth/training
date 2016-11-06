package com.training.scalafortheimpatient.chap12

object Chap12 {

}

object Ex1 extends App {
  def values(fun: (Int) => Int, low: Int, high: Int): Seq[(Int, Int)] = {
    if (low > high) Seq.empty
    else {
      val pair = (low, fun(low).abs)
      if (low == high) Seq(pair) else pair +: values(fun, low + 1, high)
    }
  }

  println(values(x => x * x, -5, 5))
}

object Ex2 extends App {
  println(Array(3, 2, 5, 6, 1).max)
  //  println(Array(3,2,5,6,1).reduceLeft(_ max _))
}

object Ex3 extends App {
  //  def factorial(x: Int) : Int = if(x < 1) 1 else (1 to x).reduceLeft(_ * _)
  def factorial(x: Int): Int = if (x < 1) 1 else (1 to x).product

  println(factorial(0))
  println(factorial(1))
  println(factorial(3))
}

object Ex4 extends App {
  //  def factorial(x: Int) : Int = (1 to x).foldLeft(1)(_ * _)
  def factorial(x: Int): Int = (1 to x).product

  println(factorial(0))
  println(factorial(1))
  println(factorial(3))
}

object Ex5 extends App {
  def largest(fun: (Int) => Int, inputs: Seq[Int]) = {
    //    inputs.map(fun).reduceLeft(_ max _)
    inputs.map(fun).max
  }

  println(largest(x => 10 * x - x * x, 1 to 10))
}

object Ex6 extends App {
  def largest(fun: (Int) => Int, inputs: Seq[Int]) = {
    if (inputs.isEmpty) -1
    else {
      val index = inputs.map(fun).zipWithIndex
        .reduceLeft { (x, y) =>
        val max = x._1 max y._1
        if (max == x._1) x else y
      }._2
      inputs(index)
    }
  }
  println(largest(x => 10 * x - x * x, Seq.empty))
  println(largest(x => 10 * x - x * x, 1 to 10))
}

object Ex7 extends App {
  def adjustToPair(f: (Int, Int) => Int)(pair: (Int,Int)) = f(pair._1, pair._2)
//def adjustToPair(f: (Int, Int) => Int) = (pair: (Int,Int)) => f(pair._1, pair._2)
  println(adjustToPair(_ * _)((6,7)))
  println(((1 to 10) zip (11 to 20)).map(adjustToPair(_ * _)))
}

object Ex8 extends App {
  val a = Array("a", "ab", "abc")
  val b = Array(1,2,3)

  println(a.corresponds(b)((x,y) => x.length == y))
}

object Ex9 extends App {
  def corresponds[T,U](left:Array[T], right:Array[U], f:(T,U) => Boolean) : Boolean = {
    left.zip(right).forall(pair => f(pair._1, pair._2))
  }

  val a = Array("a", "ab", "abc")
  val b = Array(1,2,3)

  println("Result " + corresponds(a, b, (x:String,y:Int) => x.length == y))
}

object Ex10 extends App {
  def unless(condition: => Boolean)(block: => Unit) = {
    if(!condition) block
  }
  val b = false
  unless(b) { println("Execute Block") }
}