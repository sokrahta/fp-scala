//package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] = 
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def matchExercise(ints: List[Int]): Int = ints match {
		case Cons(x, Cons(2, Cons(4, _))) => x
		case Nil => 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
		case Cons(h, t) => h + sum(t)
		case _ => 101
	}

	def tail[A](as: List[A]): List[A] = as match {
		case Nil => Nil
		case Cons(h, t) => t
	}

	def setHead[A](as :List[A], a: A): List[A] = as match {
		case Nil => Nil
		case Cons(h, t) => Cons(a, t)
	}

	@annotation.tailrec
	def drop[A](l: List[A], n: Int): List[A] = {
		if (n <= 0) l
		else drop(tail(l), n-1)
	}

	@annotation.tailrec
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(h, t) => if (f(h)) dropWhile(t, f)
			else l
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
		case Nil => a2
		case Cons(h, t) => Cons(h, append(t, a2))
	}

	def init[A](l: List[A]): List[A] = {
		//@annotation.tailrec
		def go(as: List[A]): List[A] = as match {
			case Nil => Nil
			case Cons(h, Cons(t, Nil)) => Cons(h, Nil)
			case Cons(h, t) => Cons(h, go(t))
		}
		go(l)
	}

	def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	def sum2(ns: List[Int]) = 
		foldRight(ns, 0)((x,y) => x + y)

	def product2(ns: List[Double]) =
		foldRight(ns, 1.0)(_ * _)

	def foldRightSC[A,B](as: List[A], z: B, g: (A) => Boolean, y: B)(f: (A,B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => if (g(x)) y
			else f(x, foldRightSC(xs, z, g, y)(f))
	}

	def foldExercise(): List[Int] = {
		foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
	}

	def lengthR[A](as: List[A]): Int = as match {
		case Nil => 0
		case Cons(h, t) => 1 + lengthR(t)
	}

	def length[A](as: List[A]): Int = {
		foldRight(as, 0)((x,y) => y + 1)
	}

	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
		case Nil => z
		case Cons(h, t) => foldLeft(t, f(z, h))(f)
	}

	def sumLeft(ns: List[Int]): Int =
		foldLeft(ns, 0)(_ + _)

	def productLeft(ns: List[Double]): Double =
		foldLeft(ns, 1.0)(_ * _)

	def lengthLeft[A](ns: List[A]): Int =
		foldLeft(ns, 0)((x,y) => x + 1)

	def reverse[A](as: List[A]): List[A] = 
		foldLeft(as, Nil:List[A])((x,y) => Cons(y, x))

	def foldLeftR[A,B](as: List[A], z: B)(f: (B,A) => B): B = 
		foldRight(as, (b:B) => b)((a, g) => b => g(f(b,a)))(z)

	def foldRightL[A,B](as: List[A], z: B)(f: (A,B) => B): B = 
		foldLeft(reverse(as), z)((a,b) => f(b,a))

	def appendRight[A](l1: List[A], l2: List[A]): List[A] = 
		foldRight(l1, l2)(Cons(_,_))

	def concat[A](ss: List[List[A]]): List[A] =
		foldRight(ss, Nil:List[A])(appendRight(_,_))

	//@annotation.tailrec
	def listPlusOne(ints: List[Int]): List[Int] = ints match {
		case Nil => Nil
		case Cons(h, t) => Cons(h+1, listPlusOne(t))
	}

	def listD2Str(ds: List[Double]): List[String] = ds match {
		case Nil => Nil:List[String]
		case Cons(h, t) => Cons(h.toString, listD2Str(t))
	}

	def map[A,B](as: List[A])(f: A => B): List[B] = as match {
		case Nil => Nil:List[B]
		case Cons(h, t) => Cons(f(h), map(t)(f))
	}

	def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
		case Nil => Nil:List[A]
		case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
	}

	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
		foldRight(as, Nil:List[B])((x: A, y: List[B]) => appendRight(f(x),y))

	def filterFM[A](as: List[A])(f: A => Boolean): List[A] = 
		flatMap(as)(x => if (f(x)) List(x) else Nil:List[A])

	def sumSequence(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(a,b), Cons(c,d)) => Cons(a+c, sumSequence(b,d))
	}

	def zipWith[A](l: List[A], r: List[A])(f: (A,A) => A): List[A] = (l,r) match {
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(a,b), Cons(c,d)) => Cons(f(a,c), zipWith(b,d)(f))
	}

	@annotation.tailrec
	def startsWith[A](as: List[A], sub: List[A]): Boolean = (as, sub) match {
		case (_, Nil) => true
		case (Nil, _) => false
		case (Cons(a,b), Cons(c,d)) => if (a==c) startsWith(b,d) else false
	}

	@annotation.tailrec
	def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = as match {
		case Nil => false
		case Cons(h, t) => if (startsWith(as, sub)) true
			else hasSubsequence(t, sub)
	}
}
