package com.training.scalafortheimpatient.chap11

import scala.math._

object Ex3 extends App {

  case class Fraction(n: Int, d: Int) {
    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d);
    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d);

    override def toString = num + "/" + den

    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0

    def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)

    def +(right: Fraction) = new Fraction((right.den * num) + (den * right.num), den * right.den)

    def -(right: Fraction) = new Fraction((right.den * num) - (den * right.num), den * right.den)

    def *(right: Fraction) = new Fraction(right.num * num, den * right.den)

    def /(right: Fraction) = new Fraction(right.num / num, den / right.den)
  }

  println(Fraction(5,10) + Fraction(10,10))
  println(Fraction(5,10) / Fraction(5,10))
  println(Fraction(5,10) - Fraction(5,10))
  println(Fraction(5,10) * Fraction(10,10))

}

object Ex4 extends App {
  case class Money(unit : Int, dec : Int) {
    def +(right : Money) = Money(unit + right.unit + (dec + right.dec) / 10, (dec + right.dec) % 10)
    def -(right : Money) = Money(unit - right.unit - dec / 100, dec - right.dec % 100)
    def ==(right : Money) = unit == right.unit && dec == right.dec
    def <(right : Money) = unit < right.unit || (unit == right.unit && dec < right.dec)
  }

  println(Money(2,9) + Money(3,2))
  println(Money(5,10) - Money(5,10))
  println(Money(5,10) == Money(5,10))
  println(Money(1,80) < Money(1,81))
  println(Money(1,80) < Money(1,1))
}

object Ex5 extends App {
  case class Table(rows : Array[Array[String]] = Array(Array())) {
    def |(s : String) = Table(rows.dropRight(1) :+ (rows.last :+ s))
    def ||(s : String) = Table(rows :+ Array(s))
    override def toString = rows.map(row => "<tr><td>" + row.mkString("</td><td>") + "</td></tr>").mkString("\n")
  }

  println(Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET")
}

object Ex6 extends App {
  case class Figure(rows : Array[Array[String]] = Array(Array())) {
    def >(f:Figure) = Figure(rows.zip(f.rows).map(t => t._1 ++ t._2))
    def v(f:Figure) = Figure(rows ++ f.rows)
    override def toString = rows.map(row => row.mkString).mkString("\n")
  }

  println(Figure(Array(Array("a","b","c"),Array("d","e","f"))) > Figure(Array(Array("1","2","3"),Array("4","5","6"))))
  println(Figure(Array(Array("a","b","c"),Array("d","e","f"))) v Figure(Array(Array("1","2","3"),Array("4","5","6"))))
}

object Ex7 extends App {
  case class BitSequence(mask:Long = 0) {
    def apply(i:Int) : Long = mask >> i & 1
    def update(i:Int, v:Int) = BitSequence(mask ^ (v << i))
    override def toString = mask.toBinaryString
  }

  val x = BitSequence(4)
  print(x(3)) ; print(x(2)) ; print(x(1)) ; print(x(0))
  println()
  val y = x(1) = 1
  print(y(3)) ; print(y(2)) ; print(y(1)) ; print(y(0))
}

object Ex8 extends App {
  class Matrix(val matrix:Array[Array[Int]]) {
    def this(nbCols:Int = 1, nbRows:Int = 1) = this(Array.ofDim[Int] (nbRows, nbCols))

    def apply(col:Int, row:Int) : Int = matrix(row)(col)
    def *(scalar:Int) : Matrix = new Matrix(matrix.map(_.map(_*scalar)))
    def +(scalar:Int) : Matrix = new Matrix(matrix.map(_.map(_+scalar)))

    def *(right:Matrix) : Matrix = new Matrix(matrix.zip(right.matrix).map(t => mult(t._1, t._2)))
    def +(right:Matrix) : Matrix = new Matrix(matrix.zip(right.matrix).map(t => add(t._1, t._2)))

    private def add = merge((a:Int,b:Int) => a + b) _
    private def mult = merge((a: Int, b: Int) => a * b) _

    private def merge(f:(Int,Int) => Int)(left:Array[Int], right:Array[Int]) = left.zip(right).map(t => f(t._1,t._2))

    override def toString = matrix.map(_.mkString).mkString("\n")
  }

  val m = new Matrix(2,2)
  println(m)
  println("___")
  val m2 = m + 1 * 2
  println(m2)
  println("___")
  println(m2 + m2 + m2)
  println("___")
  println(m2 * m2)

}
































