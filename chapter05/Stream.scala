sealed trait Stream[+A] {
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A] = 
		if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

	def toListStackUnsafe: List[A] = this match {
		case Cons(h, t) => h() :: t().toList
		case _ => List()
	}

	def toList: List[A] = {
		@annotation.tailrec
		def go(s: Stream[A], acc: List[A]): List[A] = this match {
			case Empty => acc
			case Cons(h, t) => go(t(), h() :: acc)
		}
		go(this, List()).reverse
	}

	def take(n: Int): Stream[A] = {
		if (n > 0) this match {
			case Empty => empty
			case Cons(h, t) => cons(h(), t().take(n-1))
		} else empty
	}

	def drop(n: Int): Stream[A] = {
		if (n > 0) this match {
			case Empty => this
			case Cons(h, t) => t().drop(n-1)
		} else this
	}

	def takeWhileER(p: A => Boolean): Stream[A] = this match {
		case Empty => empty
		case Cons(h, t) => val head = h(); if (!p(head)) empty
			else cons(head, t().takeWhile(p))
	}

	def existsER(p: A => Boolean): Boolean = this match {
		case Cons(h, t) => p(h()) || t().exists(p)
		case _ => false
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h,t) => f(h(), t().foldRight(z)(f))
		case _ => z
	}

	def exists(p: A => Boolean): Boolean = 
		foldRight(false)((a,b) => p(a) || b)

	def forAll(p: A => Boolean): Boolean = 
		foldRight(true)((a,b) => p(a) && b)

	def takeWhile(p: A => Boolean): Stream[A] = 
		foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else empty)

	def headOption(): Option[A] =
		foldRight(None: Option[A])((a,b) => Some(a))

	def map[B](f: A => B): Stream[B] = 
		foldRight(empty[B])((a,b) => cons(f(a), b))

	def filter[B](f: A => Boolean): Stream[A] = 
		foldRight(empty[A])((a,b) => 
			if (f(a)) cons(a, b)
			else b )

	def append[B>:A](as: Stream[B]): Stream[B] = 
		foldRight(as)((a,b) => cons(a,b))

	def flatMap[B](f: A => Stream[B]): Stream[B] =
		foldRight(empty[B])((a,b) => f(a) append b)

	def find(f: A => Boolean): Option[A] =
		filter(f).headOption

	def constant[A](a: A): Stream[A] = {
		lazy val cs: Stream[A] = cons(a, cs)
		cs
	}

	def from(n: Int): Stream[Int] =
		cons(n, from(n+1))

	def fibs(): Stream[Int] = {
		def go(a: Int, b: Int): Stream[Int] = 
			cons(a, go(b, a+b))
		go(0, 1)
	}

	def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] =
		f(z) match {
			case Some((a,s)) => cons(a, unfold(s)(f))
			case None => empty
		}

	def fibsUnfold(): Stream[Int] = 
		unfold((0,1)) { case (a,b) => Some((a,(b,a+b))) }

	def fromUnfold(n: Int): Stream[Int] =
		unfold(n)((a) => Some(a, a+1))

	def constantUnfold[A](a: A): Stream[A] =
		unfold(a)(a => Some(a, a))

	def onesUnfold(): Stream[Int] = 
		unfold(1)((_) => Some(1, 1))

	def mapUnfold[B](f: A => B): Stream[B] =
		unfold(this) { 
			case Cons(h,t) => Some((f(h()), t()))
			case _ => None
		}

	def takeUnfold(n: Int): Stream[A] =
		unfold((this,n)) {
			case (Cons(h,t), m) if m == 1 => Some((h(), (empty, m-1)))
			case (Cons(h,t), m) if m > 0 => Some((h(), (t(), m-1)))
			case _ => None
		}

	def takeWhileUnfold(p: A => Boolean): Stream[A] = 
		unfold((this,p)) {
			case (Cons(h,t), f) => val head = h(); if (f(head)) Some((head, (t(),f)))
				else None
			case _ => None
		}

	def zipWithUnfold[B,C](other: Stream[B])(f: (A,B) => C): Stream[C] = 
		unfold((this,other)) {
			case (Cons(a,b), Cons(c,d)) => Some( f(a(),c()), ((b(),d())) )
			case _ => None
		}

	def zipAllUnfold[B](other: Stream[B]): Stream[(Option[A],Option[B])] =
		unfold((this,other)) {
			case (Cons(a,b), Cons(c,d)) => Some( (Some(a()),Some(c())), ((b(),d())) )
			case (Cons(a,b), Empty) => Some( (Some(a()),None), ((b(),empty)) )
			case (Empty, Cons(c,d)) => Some( (None,Some(c())), ((empty,d())) )
			case _ => None
		}

	def startsWith[A](s: Stream[A]): Boolean =
		zipAllUnfold(s).takeWhileUnfold(!_._2.isEmpty) forAll { case (a,b) => a==b }

	def tails: Stream[Stream[A]] =
		unfold(this) {
			case Empty => None
			case a => Some((a, a drop 1))
		} append (empty)

	def hasSubsequence[A](s: Stream[A]): Boolean =
		tails exists (_ startsWith s)

	def scanRight[B,C](z: B)(f: (A,=>B) => B): Stream[B] =
		foldRight((z, cons(z,empty)))((a,p) => {
			val b2 = f(a,p._1)
			(b2, cons(b2,p._2))
		})._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
}
