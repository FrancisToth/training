package com.training.functionalprogramminginscala.chapter2

/* Write a recursive function to get the nth Fibonacci number. The first two Fibonacci numbers
* are 0 and 1. The nth number is always the sum of the previous two-the sequence begins
* 0, 1, 1, 2, 3, 5. Your definition should use a local tail-recursive function. */
object Ex2_1 extends App {
  def nthFibonacci(n: Int): Int = {
    @annotation.tailrec
    def nextNumber(a: Int, b:Int, index: Int): Int = {
      if(index == n) a
      else nextNumber(b, a+b, index+1)
    }
    nextNumber(0, 1, 1)
  }
}
