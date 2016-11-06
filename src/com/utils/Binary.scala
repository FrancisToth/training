package com.utils

object Binary {
  def toBinary(n: Int): List[Int] = {
    def _toBinary(n: Int, acc: List[Int] = List()): List[Int] = {
      val binary = if (n % 2 == 0) 0 else 1
      val half = n / 2
      if (half == n) binary :: acc else _toBinary(half, binary :: acc)
    }

    _toBinary(n) match {
      case xs@(0 :: Nil) => xs
      case 0 :: xs => xs
      case xs => xs
    }
  }
}
