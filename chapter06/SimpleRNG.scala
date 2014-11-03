trait RNG {
	def nextInt: (Int, RNG)
	def nextInt(ceil: Int): (Int, RNG)
	def nextDouble: (Double, RNG)
	def nextIntDouble: ((Int, Double), RNG)
	def nextDoubleInt: ((Double, Int), RNG)
	def nextDouble3: ((Double, Double, Double), RNG)
	def nextIntN(count: Int): (List[Int], RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
	def nextInt: (Int, RNG) = {
		val newSeed = (seed * 0x5deece66dl + 0xbL) & 0xffffffffffffL
		val nextRNG = SimpleRNG(newSeed)
		val n = (newSeed >>> 16).toInt
		(n, nextRNG)
	}

	def nextInt(ceil: Int): (Int, RNG) = {
		val (i, r) = nextInt
		(if (i < 0) -(i+1) else i, r)
	}

	def nextDouble: (Double, RNG) = {
		val (i, r) = nextInt(Int.MaxValue)
		(i.toDouble / (Int.MaxValue.toDouble + 1), r)
	}

	def nextIntDouble: ((Int, Double), RNG) = {
		val (i, r1) = nextInt
		val (d, r2) = r1.nextDouble
		((i,d), r2)
	}

	def nextDoubleInt: ((Double, Int), RNG) = {
		val (d, r1) = nextDouble
		val (i, r2) = r1.nextInt
		((d,i), r2)
	}

	def nextDouble3: ((Double, Double, Double), RNG) = {
		val (d1, r1) = nextDouble
		val (d2, r2) = r1.nextDouble
		val (d3, r3) = r2.nextDouble
		((d1,d2,d3), r3)
	}

	def nextIntN(count: Int): (List[Int], RNG) = {
		@annotation.tailrec
		def go(n: Int, acc:List[Int], r: RNG): (List[Int], RNG) = {
			if (n <= 0) (acc, r) else {
				val (i, s) = r.nextInt
				go(n-1, i::acc, s)
			}
		}
		go(count, Nil, this)
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

	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = 
		rng => {
			val (a, r1) = ra(rng)
			val (b, r2) = rb(r1)
			(f(a,b), r2)
		}

	def nonNegativeEven: Rand[Int] = 
		map(int)(i => i - i % 2)

	def double: Rand[Double] = 
		map(int)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

	def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = 
		map2(ra,rb)((_,_))

	val randIntDouble: Rand[(Int,Double)] = 
		both(int, double)

	val randDoubleInt: Rand[(Double,Int)] =
		both(double, int)

	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
		fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_::_))

	def intsFold(count: Int): Rand[List[Int]] = 
		sequence(List.fill(count)(int))
}
