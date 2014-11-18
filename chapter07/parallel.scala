// end goal: map function to list in parallel
// val outputList = parMap(inputList)(f)

def sum(ints: Seq[Int]): Int = 
	ints.foldLeft(0)((a,b) => a+b)

def sum(ints: IndexedSeq[Int]): Int =
	if (ints.size <= 1)
		ints.headOption getOrElse 0
	else {
		val (l,r) = ints.splitAt(ints.length/2)
		val sumL: Par[Int] = Par.unit(sum(l))
		val sumR: Par[Int] = Par.unit(sum(r))
		Par.get(sumL) + Par.get(sumR)
	}

trait Par[+A]
object Par {
	def unit[A](a: => A): Par[A] = ???
	def get[A](a: Par[A]): A = ???
	def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???
}
