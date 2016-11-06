package com.training.algorithm.sorting

import scala.util.{ Random => Rnd }

object Random {

  def randomIntArray(size: Int = 10, upperBound: Int = 10): Array[Int] = {
    val randomizer = Rnd
    val arr = new Array[Int](size)
    for(i <- arr.indices) {
      arr(i) = randomizer.nextInt(upperBound)
    }
    arr
  }
}
