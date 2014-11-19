// end goal: map function to list in parallel
// val outputList = parMap(inputList)(f)

def sum(ints: Seq[Int]): Int = 
	ints.foldLeft(0)((a,b) => a+b)

def sum(ints: IndexedSeq[Int]): Par[Int] =
	if (ints.size <= 1)
		Par.unit(ints.headOption getOrElse 0)
	else {
		val (l,r) = ints.splitAt(ints.length/2)
		Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_+_)
	}

trait Par[+A]
object Par {
	def unit[A](a: => A): Par[A] = ???
	def run[A](a: Par[A]): A = ???
	def fork[A](a: => Par[A]): Par[A] = ???
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
	def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???
}

Par.map2(Par.unit(1), Par.unit(1))(_+_)
