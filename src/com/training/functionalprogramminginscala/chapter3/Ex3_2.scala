package com.training.functionalprogramminginscala.chapter3

object Ex3_2 extends App {
  def tail[A](list: List[A]): List[A] = list match {
    case h :: t => t
    case _ => List.empty[A]
  }

  assert(tail(List(1,2,3)) == List(2,3))
  assert(tail(List(1)) == List.empty)
  assert(tail(List()) == List.empty)
}
