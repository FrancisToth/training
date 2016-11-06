package com.training.scalafortheimpatient.chap8

object Ex5 {
  case class Point(x : Double, y : Double)
  class LabeledPoint(label : String, x : Double, y : Double) extends Point(x,y)

  abstract class Shape {
    def centerPoint : Point
  }

  class Rectangle(topLeft : Point, bottomRight : Point) extends Shape {
    override def centerPoint: Point = Point(
      topLeft.x + (bottomRight.x - topLeft.x)/2,
      bottomRight.y + (topLeft.y - bottomRight.y)/2
    )
  }

  class Circle(origin : Point, radius : Double) extends Shape {
    override def centerPoint: Point = origin
  }

  class Square(topLeft : Point = Point(0,0), width : Double = 0) extends java.awt.Rectangle {
    def this(width : Double) = this(Point(0,0), width)
  }

  class Creature {
    def range: Int = 10
    val env: Array[Int] = new Array[Int](range)
    override def toString = s"Creature(${env.length} => $range)"
  }

  class Ant extends Creature {
    override def range = 2
  }

  class Truc private(private val a : Int)
}