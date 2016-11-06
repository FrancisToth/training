package com.training.scalafortheimpatient.chap10

import java.awt.Point

import scala.language.reflectiveCalls

object Ex1 {

  /*trait RectangleLike {
    def translate(x:Int, y:Int):Unit = ???
    def grow(x:Int, y:Int):Unit = ???
  }

  val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
  egg.translate(10, -10)
  egg.grow(10, 20)

  class OP extends java.awt.Point with scala.math.Ordered[java.awt.Point] {
    override def compare(that: Point): Int = {
      if(x == that.x && y == that.y) 0
      else if(x <= that.x && y <= that.y) 1
      else -1
    }
  }

  //lin(BitSet) = lin(BitSetFactory) >> lin(AnyRef)
  //lin(BitSet) = lin(AnyRef) >> lin(BitSetLike) >> lin(AnyRef)
  //lin(BitSet) = lin(AnyRef) >> lin(SortedSet) >> lin(AnyRef) lin(SortedSetLike) >> lin(AnyRef)
  //lin(BitSet) = lin(AnyRef) >> lin(SortedSet) >> lin(AnyRef) >> lin(SetLike) >> lin(Sorted) >> lin(AnyRef) >> lin(SortedSetLike) >> lin(AnyRef)
  //lin(SortedSet) >> lin(SortedSetLike) >> lin(Set) >> lin(AnyRef)

  trait CryptoLogger {
    def encrypt(string : String) : String = string.map(c => (c - key).toChar).mkString("")
    def key : Int = 3
  }

  class Test extends CryptoLogger {
    override def key : Int =  2
  }

  trait PropertyChangeSupport extends java.beans.PropertyChangeSupport
  val p = new Point() with PropertyChangeSupport

  trait Logger {
    this: { def write(msg : String) } =>
    def logError(msg : String): Unit = {
      write(msg)
    }
  }

  class Truc extends Logger {
    def write(msg : String): Unit = {

    }
  }*/
}

