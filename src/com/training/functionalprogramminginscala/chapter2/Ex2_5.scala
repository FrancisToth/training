package com.training.functionalprogramminginscala.chapter2

object Ex2_5 extends App {
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a:A) => f(g(a))

  val multiplyBy2 = (x:Int) => x * 2
  val subtractOne = (x:Int) => x - 1

  val f = compose(multiplyBy2, subtractOne)
  assert(f(2) == 2)
  assert(f(6) == 10)
  assert(f(1) == 0)
}
