package com.training.scalafortheimpatient.chap9

import java.io.PrintWriter

import scala.io.Source

object Ex1 extends App {
  println(Source.fromFile("/Users/francistoth/Desktop/test.json").getLines().toArray.reverse)
}

object Ex1 extends App {
  println(Source.fromFile("/Users/francistoth/Desktop/test.json").getLines().toArray.reverse)
}

object Ex2 extends App {
  val writeFunc = writeFile(filePath) _

  writeFunc("\ta\n" * 5)
  writeFunc(replaceTabs(filePath).mkString("\n"))
  println(readFile(filePath))

  def replaceTabs(filePath: String, nb: Int = 10): Array[String] = {
    Source.fromFile(filePath).getLines().toArray.map( """\t""".r.replaceAllIn(_, " " * nb))
  }
}

object Ex3 extends App {
  writeFile(filePath)(("a" * 12 + "\n") * 5 + ("b" * 13 + "\n") * 2)
  println(Source.fromFile(filePath).getLines().toArray.filter(_.length > 12).mkString("\n"))
}

object Ex4 extends App {
  writeFile(filePath)("1.0\n2.0\n3.0\n")
  val doubles = Source.fromFile(filePath).getLines().toArray.map( """\s""".r.replaceAllIn(_, "").toDouble)
  println("avg:%.2f sum:%.2f min:%.2f max:%.2f".format(
    doubles.sum / doubles.length,
    doubles.sum,
    doubles.min,
    doubles.max))
}