package com.training.careercup.moderate

// Write a function to swap a number in place (that is without temporary variables)
object Ex_16_1_NumberSwapper extends App {

  def solution(a: Int, b: Int): (Int, Int) = {
    // We cannot reassign to val, so arguments are stored in vars.
    var (a2, b2) = (a, b)
    a2 = a2 ^ b2
    b2 = a2 ^ b2
    a2 = a2 ^ b2
    (a2, b2)
  }

//  def assertSolutionIsEqualTo(expected): Unit = {
//
//  }

  println(solution(42, 365))
}
