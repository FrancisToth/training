package com.training.codibility

object Random {
  def randomArray[T: Randomizer] = implicitly[Randomizer[T]].apply()
}

trait Randomizer[T] { def apply(): T }

object Randomizer {

  val randomizer = new java.util.Random()

  val randomInt = new Randomizer[Int] { override def apply(): Int = randomizer.nextInt() }
}
