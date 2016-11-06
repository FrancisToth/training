package com.training.functionalprogramminginscala.chapter3

object Ex3_6 extends App {
  def init[A](l: List[A]): List[A] = l match {
    case Nil => List.empty[A]
    case h :: Nil => List.empty[A]
    case h :: t => h :: init(t)
  }

  assert(init(List(1, 2, 3)) == List(1, 2))
  assert(init(List(1)) == List.empty)
  assert(init(List()) == List.empty)
}
