package com.training.algorithm.sorting

object Sorting {

  // O(n^2) / Memory O(1)
  def bubbleSort(ints: Array[Int]): Array[Int] = {
    for(i <- ints.indices) {
      for(j <- ints.indices) {
        if(ints(i) < ints(j)) {
          swap(ints, i, j)
        }
      }
    }
    ints
  }

  // O(n^2) / Memory O(1)
  def selectionSort(ints: Array[Int]): Array[Int] = {
    for(i <- ints.indices) {
      var min = i
      for(j <- (i+1) until ints.length) {
        min = if(ints(min) > ints(j)) j else min
      }
      if(min != i)
        swap(ints, min, i)
    }
    ints
  }

  // O(n lg n) / Memory depends
  def mergeSort(arr: Array[Int]): Array[Int] = {
    val helper = new Array[Int](arr.length)
    _mergeSort(arr, helper, 0, arr.length - 1)

    def _mergeSort(arr: Array[Int], helper: Array[Int], low: Int, high: Int): Unit = {
      if(low < high) {
        val middle = (low + high) / 2
        _mergeSort(arr, helper, low, middle)
        _mergeSort(arr, helper, middle + 1, high)
        _merge(arr, helper, low, middle, high)
      }
    }

    def _merge(arr: Array[Int], helper: Array[Int], low: Int, middle: Int, high: Int): Unit = {
      for(i <- low to high) {
        helper(i) = arr(i)
      }

      var helperLeft, current = low
      var helperRight = middle + 1

      while(helperLeft <= middle && helperRight <= high) {
        if(helper(helperLeft) <= helper(helperRight)) {
          arr(current) = helper(helperLeft)
          helperLeft += 1
        } else {
          arr(current) = helper(helperRight)
          helperRight += 1
        }
        current += 1
      }

      val remaining = middle - helperLeft
      for(i <- 0 to remaining) {
        arr(current + i) = helper(helperLeft + i)
      }
    }

    arr
  }

  // AVG : O(n log n) WORST : O(n^2) / Memory O(lg n)
  def quickSort(arr: Array[Int]): Array[Int] = {

    def _quickSort(arr: Array[Int], left: Int, right: Int): Array[Int] = {
      val index = _partition(arr, left, right)
      if(index - 1 > left) _quickSort(arr, left, index - 1)
      if(index < right) _quickSort(arr, index, right)
      arr
    }

    def _partition(arr: Array[Int], l: Int, r: Int): Int = {
      var (left, right) = (l, r)
      val pivot = arr((left + right) / 2)
      while(left <= right) {
        while(arr(left) < pivot) left += 1
        while(arr(right) > pivot) right -= 1

        if (left <= right) {
          swap(arr, left, right)
          left += 1
          right -= 1
        }
      }
      left
    }

    _quickSort(arr, 0, arr.length - 1)
  }

  def radix(arr: Array[Int]): Array[Int] = ???

  def swap[A](arr: Array[A], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }
}
