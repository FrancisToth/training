package com.training.codibility

object NumberDecimalZip extends App {

  def solution(a: Int, b: Int): Int = {
    val a2s = a.toString
    val b2s = b.toString

    def _remainder(left: String, right: String): String = {
      val diff = Math.abs(a2s.length - b2s.length)
      (if (a2s.length < b2s.length)
        b2s.splitAt(b2s.length - diff)
      else
        a2s.splitAt(a2s.length - diff))._2
    }

    if(a2s.length + b2s.length >= 9) -1
    else {
      val left = a2s.zip(b2s).map{ case (l,r) => s"$l$r" }.mkString
      val remainder = _remainder(a2s, b2s)

      (left + remainder).toInt
    }
  }

  println(solution(1234,5678))
  //println(100000000)
}


