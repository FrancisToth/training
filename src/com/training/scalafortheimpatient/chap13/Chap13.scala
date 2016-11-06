package com.training.scalafortheimpatient.chap13

import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable._
import scala.collection.mutable
import scala.collection.mutable.{Map => mMap, SortedSet => mSortedSet, HashMap => mHashMap, LinkedList => mLinkedList}

object Chap13 {
}

object Ex1 extends App {
  def indexes(string:String) : mMap[Char, mSortedSet[Int]] = {
    val map = mMap[Char, mSortedSet[Int]]()
    for((c,i) <- string.zipWithIndex)
      map(c) = map.getOrElse(c, mSortedSet[Int]()) += i
    map
  }

  indexes("Mississippi").foreach{ (pair:(Char,mSortedSet[Int])) =>
    println(pair._1 + ":" + pair._2.mkString(","))
  }
}

object Ex2 extends App {
  def indexes(string:String) : Map[Char, List[Int]] = {
    var map = Map[Char, List[Int]]()
    for((c,i) <- string.zipWithIndex)
      map = map + (c -> (i::map(c)))
    map
  }

  indexes("Mississippi").foreach{ (pair:(Char,List[Int])) =>
    println(pair._1 + ":" + pair._2.mkString(","))
  }
}

object Ex3 extends App {
  val list = mLinkedList(0,1,2,3,0,4,5,0)
  println(list.filter(_ != 0))
}

object Ex4 extends App {
  def fun(strings: Array[String], stringToInt: Map[String, Int]) : Array[Int] = {
    strings.flatMap(stringToInt.get)
  }
  println(fun(Array("Tom", "Fred", "Harry"), Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)).mkString)
}

object Ex5 extends App {
  def mkString(t:Array[Any], sep:String = "") : String = {
    t.map(_.toString).reduceLeft { (result, e) =>
      result + (if(result.nonEmpty) sep + e else e)
    }
  }
  println(mkString(Array("Tom", "Fred", "Harry"), ","))
}

object Ex6 extends App {
  println(3::2::1::List[Int]())

  val lst = List(1,2,3,4,5,6)
  println((lst :\ List[Int]())(_ :: _).mkString(",")) // 1,2,3,4,5,6
  println((List[Int]() /: lst)(_ :+ _).mkString(",")) // 1,2,3,4,5,6
  println((lst :\ List[Int]())((i, l) => l :+ i).mkString(",")) // 6,5,4,3,2,1
}

object Ex7 extends App {
  val prices = Array(1,2,3,4,5)
  val quantities = Array(10,20,30,40,50)

  (prices zip quantities).map { p => p._1 * p._2 }

  val multiply = ((x:Int,y:Int) => x*y).tupled
  println((prices zip quantities).map(multiply).mkString(","))
}

object Ex8 extends App {
  def partition(array: Array[Int], cols: Int) : Array[Array[Int]] = array.grouped(cols).toArray
  var arr = Array(1,2,3,4,5,6)
  partition(arr, 3).foreach { arr => println(arr.mkString(",")) }
}

object Ex9 extends App {
  val freq = new mHashMap[Char, Int] with mutable.SynchronizedMap[Char, Int]

  freq('c') = freq.getOrElse('c', 0) + 1

  import scala.collection.JavaConversions._
  val freq2 : scala.collection.mutable.ConcurrentMap[Char, Int] =
    new java.util.concurrent.ConcurrentHashMap[Char, Int]
}

object Ex10 extends App {
  // parallel  + aggregate
}