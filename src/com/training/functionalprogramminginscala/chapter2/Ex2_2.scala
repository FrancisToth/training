package com.training.functionalprogramminginscala.chapter2

object Ex2_2 extends App {
  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      val j = i + 1
      if (j >= as.length) true
      else if (ordered(as(i), as(j))) loop(j)
      else false
    }
    loop(0)
  }

  assert(isSorted(Array(1, 2, 3, 4))(_ < _))
  assert(!isSorted(Array(1, 2, 3, 4))(_ > _))

  assert(!isSorted(Array(4, 2, 1, 3))(_ < _))
  assert(isSorted(Array[Int]())(_ > _) == isSorted(Array[Int]())(_ < _))
}
