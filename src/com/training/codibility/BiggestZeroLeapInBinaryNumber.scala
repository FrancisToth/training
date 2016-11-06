package com.training.codibility

import com.utils.Binary.toBinary

object BiggestZeroLeapInBinaryNumber extends App {
  def solution(n: Int): Int = {
    val binary = toBinary(n)
    divide(binary, 0, binary.length - 1)._2
  }

  def divide(binary: List[Int], low: Int, high: Int): (Int, Int) = {
    if (low < high) {
      val mid = (low + high) / 2

      val (lIdx, lMax) = divide(binary, low, mid)
      val (rIdx, rMax) = divide(binary, mid + 1, high)

      if((lIdx + lMax) == rIdx) (lIdx, lMax + rMax)
      else if(lMax > rMax) (lIdx, lMax)
      else (rIdx, rMax)
    }
    else if(binary(low) == 0) (low, 1) else (low, 0)
  }

  def assertSolutionIsEqualTo(n: Int, expected: Int): Unit = {
    val actual = solution(n)
    println(toBinary(n), solution(n))
    assert(actual == expected, s"solution for $n expected is : $expected. Was : $actual")
  }

//  assertSolutionIsEqualTo(100, 2)
//  assertSolutionIsEqualTo(99, 3)
//  assertSolutionIsEqualTo(63, 0)
//  assertSolutionIsEqualTo(1, 0)
//  assertSolutionIsEqualTo(6, 0)
//  assertSolutionIsEqualTo(16, 0)
//  assertSolutionIsEqualTo(1024, 0)
//  assertSolutionIsEqualTo(1162, 0)
//  assertSolutionIsEqualTo(51712, 2)
//  assertSolutionIsEqualTo(20, 2)
  assertSolutionIsEqualTo(66561, 9)



//  val f = toBinary() _
//  (1 to 100).foreach { n =>
//    println(s"$n : ${f(n)} -> ${solution(n)}")
//  }
}