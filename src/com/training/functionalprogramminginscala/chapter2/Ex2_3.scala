package com.training.functionalprogramminginscala.chapter2

object Ex2_3 extends App {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b)

  val multiply = (a:Int, b:Int) => a * b
  assert(curry(multiply)(2)(3) == multiply(2,3))
}
