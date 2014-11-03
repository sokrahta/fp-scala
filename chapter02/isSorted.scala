def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
	@annotation.tailrec
	def go(n: Int, as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		if (n+1 >= as.length) true
		else if (ordered(as(n), as(n+1))) go(n+1, as, ordered)
		else false
	}

	go(0, as, ordered)
}
