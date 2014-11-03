//package fpinscala.state

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
	val (i,r) = rng.nextInt
	(if (i<0) -(i+1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
	val (i,r) = rng.nextInt
	(i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
	val (i, r1) = rng.nextInt
	val (d, r2) = double(r1)
	((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
	val ((i,d), r) = intDouble(rng)
	((d,i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
	val (d1,r1) = double(rng)
	val (d2,r2) = double(r1)
	val (d3,r3) = double(r2)
	((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
	@annotation.tailrec
	def go(n: Int, r: RNG, acc: List[Int]): (List[Int], RNG) =
	  if (n <= 0) (acc, r)
	  else {
		val (i2, r2) = r.nextInt
		go(n-1, r2, i2 :: acc)
	  }
	go(count, rng, List())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
	rng => {
	  val (a, r1) = ra(rng)
	  val (b, r2) = rb(r1)
	  (f(a,b), r2)
	}
  
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
	  val (a, r1) = f(rng)
	  g(a)(r1)
	}
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
	  val (a, s1) = run(s)
	  f(a).run(s1)
	})
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
  def sequence[S,A](ss: List[State[S,A]]): State[S, List[A]] =
	ss.foldRight(unit[S,List[A]](List()))((f, acc) => f.map2(acc)(_::_))
  def modify[S](f: S => S): State[S,Unit] = for {
	s <- get
	_ <- set(f(s))
  } yield ()
  def get[S]: State[S,S] = State(s => (s,s))
  def set[S](s: S): State[S,Unit] = State(_ => ((), s))
}

object State {
  type Rand[A] = State[RNG, A]
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
  def sequence[S,A](ss: List[State[S,A]]): State[S, List[A]] =
	ss.foldRight(unit[S,List[A]](List()))((f, acc) => f.map2(acc)(_::_))
  def modify[S](f: S => S): State[S,Unit] = for {
	s <- get
	_ <- set(f(s))
  } yield ()
  def get[S]: State[S,S] = State(s => (s,s))
  def set[S](s: S): State[S,Unit] = State(_ => ((), s))
  
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
	_ <- sequence(inputs.map(i => modify((s: Machine) => (i,s) match {
	  case (_, Machine(_, 0, _)) => s
	  case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins+1)
	  case (Coin, Machine(false, _, _)) => s
	  case (Turn, Machine(true, _, _)) => s
	  case (Turn, Machine(false, candies, coins)) => Machine(true, candies-1, coins)
	})))
	s <- get
  } yield (s.coins, s.candies)
  
  override def toString = locked.toString + " " + candies.toString + " " + coins.toString
}