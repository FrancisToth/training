package com.training.functionalprogramminginscala

object chapter6 extends App {

  type Rand[+A] = RNG => (A, RNG)

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  /* Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
     Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative
     counterpart. */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /* Write a function to generate a Double between 0 and 1, not including 1. Note: You can use Int.MaxValue to obtain
     the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double. */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /* Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
     You should be able to reuse the functions you’ve already written. */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, r) = rng.nextInt
    val (d, r2) = double(r)
    ((int, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((int, d), r) = intDouble(rng)
    ((d, int), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /* Write a function to generate a list of random integers. */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List.empty, rng)
    else {
      val (xs, r1) = rng.nextInt
      val (x, r2) = ints(count - 1)(r1)
      (xs :: x, r2)
    }
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /* Use map to reimplement double in a more elegant way. See exercise 6.2. */
  def doubleViaMap(rng: RNG): (Double, RNG) = map(int)(_ / (Int.MaxValue.toDouble + 1))(rng)

  /* Write the implementation of map2 based on the following signature. This function takes two actions, ra and rb,
     and a function f for combining their results, and returns a new action that combines them: */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng) => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /* Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them. Implement
     sequence for combining a List of transitions into a single transition. Use it to reimplement the ints function you
     wrote before. For the latter, you can use the standard library function List.fill(n)(x) to make a list with x
     repeated n times. */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List.empty[A])) {
    map2(_, _)(_ :: _)
  }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  /* Implement flatMap, and then use it to implement nonNegativeLessThan. */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  /* Reimplement map and map2 in terms of flatMap. The fact that this is possible is what we’re referring to when we
     say that flatMap is more powerful than map and map2. */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a => map(rb) { b => f(a, b) } }
}

/* Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods on the State case class
   where possible. Otherwise you should put them in a State companion object. */
object State {
  def unit[A, S](a: A) = State((s: S) => (a, s))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[List[A], S](List.empty[A])) { (st, acc) => st.map2(acc)(_ :: _) }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State((s: S) => {
    val (a, s1) = run(s)
    (f(a), s1)
  })

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = State((s: S) => {
    val (a, s1) = run(s)
    val (b, s2) = rb.run(s1)
    (f(a, b), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

/* Hard: To gain experience with the use of State, implement a finite state automaton that models a simple candy
   dispenser. The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
   It can be in one of two states: locked or unlocked. It also tracks how many candies are left and how many coins it
   contains.

   The rules of the machine are as follows:
   - Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
   - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
   - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
   - A machine that’s out of candy ignores all inputs.

   The method simulateMachine should operate the machine based on the list of inputs and return the number of coins
   and candies left in the machine at the end. For example, if the input Machine has 10 coins and 5 candies, and a
   total of 4 candies are successfully bought, the output should be (14, 1). */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

import com.training.functionalprogramminginscala.State._

object Candy extends App {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def update(input: Input, s: Machine) = { (input, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candies, coins)) if candies > 0 => Machine(false, candies, coins + 1 )
        case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
      }
    }

    // Tout l'idée est de mapper les inputs sur une fonction de changement d'état
    // La fonction, une fois le run lancé, renvoie l'état (via get), fait un flatMap dessus et
    // set le bon état.
    // Une fois mappé, on transforme la liste[State[Machine, Unit]] en un state de [Machine, List[Unit]]
    val state: State[Machine, List[Unit]] = sequence(inputs.map { input =>
      get[Machine].flatMap { (machine: Machine) => set(update(input, machine)) }
    })

    // On peut désormais faire un flatMap dessus, et renvoyer une fonction de transition prenant
    // une machine en entrée et qui renvoie le total de coins/candies
    state.flatMap(_ => get.map(s => (s.coins, s.candies)))
  }
}