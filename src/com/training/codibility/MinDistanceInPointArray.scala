package com.training.codibility

import scala.annotation.tailrec


object MinDistanceInPointArray extends App {

  type Path = (Int, Int, Int)

  def solution(a: Array[Int]): Int = {
    if(a.length < 2) a.headOption.getOrElse(-1)
    else {
      def _minDistance(left: Int, right: Int): Int = Math.min(left, right)
      def _distance(start: Int, end: Int) = Math.abs(start - end)

      @tailrec
      def _solution(xs: Array[Int], i: Int = 1, minDistance: Int = 1000000): Int =
        if (i < a.length) {
          val distance = _minDistance(_distance(xs(i - 1), xs(i)), minDistance)
          _solution(xs, i + 1, distance)
        }
        else minDistance

      _solution(a.sorted)
    }
  }

  def assertSolutionIsEqualTo(arr: Array[Int], expectedResult: Int): Unit = {
    val actualResult = solution(arr)
    assert(actualResult == expectedResult, s"solution for $arr expected is : $expectedResult. Was : $actualResult")
  }

  assertSolutionIsEqualTo(Array(8, 24, 3, 20, 1, 17), 2)
  assertSolutionIsEqualTo(Array(7, 21, 3, 42, 3, 7), 0)
  assertSolutionIsEqualTo(Array(0, 100000001), 1000000)

  println(20 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22, 78, 53, 66, 71, 41, 12, 24, 31)))
  println(19 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22, 78, 53, 66, 71, 41, 12, 24)))
  println(18 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22, 78, 53, 66, 71, 41, 12)))
  println(17 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22, 78, 53, 66, 71, 41)))
  println(16 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22, 78, 53, 66, 71)))
  println(15 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22, 78, 53, 66)))
  println(14 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22, 78, 53)))
  println(13 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22, 78)))
  println(12 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73, 22)))
  println(11 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57, 73)))
  println(10 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86, 57)))
  println(9 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98, 86)))
  println(8 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6, 98)))
  println(7 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9, 6)))
  println(6 + " " + solution(Array(7, 21, 3, 42, 109, 16, 9)))
  println(5 + " " + solution(Array(7, 21, 3, 42, 109, 16)))
  println(4 + " " + solution(Array(7, 21, 3, 42, 109)))
  println(3 + " " + solution(Array(7, 21, 3, 42)))
  println(2 + " " + solution(Array(7, 21, 3)))
  println(1 + " " + solution(Array(7, 21)))
}