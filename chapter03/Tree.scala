sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	def size[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(a,b) => 1 + size(a) + size(b)
	}

	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(a) => a
		case Branch(a,b) => maximum(a) max maximum(b)
	}

	def depth[A](t: Tree[A]): Int = {
		def go(u: Tree[A], l: Int): Tree[Int] = u match {
			case Leaf(_) => Leaf(l)
			case Branch(a,b) => Branch(go(a,l+1), go(b,l+1))
		}
		maximum(go(t, 0))
	}

	def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(a) => Leaf(f(a))
		case Branch(a,b) => Branch(map(a)(f), map(b)(f))
	}

	def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
		case Leaf(a) => f(a)
		case Branch(a,b) => g(fold(a)(f)(g), fold(b)(f)(g))
	}

	def sizeF[A](t: Tree[A]): Int = 
		fold(t)(a => 1)(_ + _ + 1)

	def maximumF(t: Tree[Int]): Int = 
		fold(t)(a => a)(_ max _)

	def depthF[A](t: Tree[A]): Int =
		fold(t)(a => 1)(_ max _ + 1)

	def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] =
		fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}
