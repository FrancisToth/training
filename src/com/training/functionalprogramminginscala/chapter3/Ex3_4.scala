package com.training.functionalprogramminginscala.chapter3

object Ex3_4 extends App {

  def tail[A](list: List[A]): List[A] = list match {
    case h :: t => t
    case _ => List.empty[A]
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n >= l.length) List.empty[A]
    else if(n == 0) l
    else drop(tail(l), n - 1)
  }

  assert(drop(List(1,2,3), 0) == List(1,2,3))
  assert(drop(List(1,2,3), 2) == List(3))
  assert(drop(List(1), 2) == List.empty)
  assert(tail(List()) == List.empty)

}
