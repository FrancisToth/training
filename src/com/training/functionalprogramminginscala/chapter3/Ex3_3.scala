package com.training.functionalprogramminginscala.chapter3

object Ex3_3 extends App {
  def setHead[A](list: List[A], a: A): List[A] = list match {
    case h :: t => a :: t
    case _ => a :: Nil
  }

  assert(setHead(List(1,2,3), 4) == List(4,2,3))
  assert(setHead(List(1), 2) == List(2))
  assert(setHead(List(), 1) == List(1))
}
