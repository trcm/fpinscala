package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  val mapDouble: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, rPrime) = double(r)
    ((i, d), rPrime)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, r) = double(rng)
    val (d1, r1) = double(r)
    val (d2, r2) = double(r1)
    ((d, d1, d2), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (List(), rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, s) = ra(rng)
    val (b, ss) = rb(s)
    (f(a, b), ss)
  }


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }
}


case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =  State(s => {
    val (a, s1) = this.run(s)
    (f(a), s1)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State( s => {
    val (a, s1) = this.run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State( s=> {
    val (a, s1) = this.run(s)
    val b = f(a).run(s1)
    b
  } )
}


object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def sequence[S, A](s: List[State[S, A]]): State[S, List[A]] =
    s.foldRight(unit[S, List[A]](List()))(???)


}
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def updateMachine(input: Input)(machine: Machine): Machine = (input, machine) match {
    case (_, Machine(s, c, coins)) if c == 0 => Machine(s,c,coins)

    case (Coin, Machine(true, c, coins)) => Machine(false, c, coins + 1)
    case (Turn, Machine(false, c, coins))  => Machine(true, c - 1, coins)

    case (Turn, Machine(true, c, coins)) => Machine(true, c, coins)
    case (Coin, Machine(false, c, coins)) => Machine(false, c, coins)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(s => {
     ???
    })

}
