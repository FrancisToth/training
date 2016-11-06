package com.training.functionalprogramminginscala.chapter3

object Ex3_5 extends App {
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case h :: t if f(h) => dropWhile(t, f)
    case h :: Nil if f(h) => List.empty[A]
    case _ => l
  }

  val isPositive = (x:Int) => x > 0
  assert(dropWhile(List(1,2,3,4), isPositive) == List.empty)
  assert(dropWhile(List(1,2,-1,3), isPositive) == List(-1, 3))
  assert(dropWhile(List(), isPositive) == List())
  assert(dropWhile(List(-1,1), isPositive) == List(-1,1))
}
