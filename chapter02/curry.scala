def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
//	def partial1(a: A): B => C = {
//		(b) => f(a, b)
//	}
//	partial1
	(a: A) => (b: B) => f(a, b)
}

def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
//	def full1(a: A, b: B): C = {
//		val g = f(a)
//		g(b)
//	}
//	full1
	(a: A, b: B) => f(a)(b)
}

def compose[A,B,C](f: B => C, g: A => B): A => C = {
//	def gf(a: A): C = {
//		f(g(a))
//	}
//	gf
	(a: A) => f(g(a))
}
