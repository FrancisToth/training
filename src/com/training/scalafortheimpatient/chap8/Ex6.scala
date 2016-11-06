package com.training.scalafortheimpatient.chap8

object Ex6 {
  abstract class Shape {
    def centerPoint : Double
    def description : String

    //override def toString = s"Item($price, $description)"
  }
}