sealed trait Option[+A] {
	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(a) => Some(f(a))
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(a) => a
	}

	def flatMap[B](f: A => Option[B]): Option[B] = 
		map(f) getOrElse None

	def orElse[B >: A](ob: => Option[B]): Option[B] = 
		this map (Some(_)) getOrElse ob

	def filter(f: A => Boolean): Option[A] = 
		flatMap(a => if(f(a)) Some(a) else None)

	def mean[A](xs: Seq[Double]): Option[Double] = 
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)

	def variance(xs: Seq[Double]): Option[Double] = 
		mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

	def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

	def map2PM[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a,b) match {
		case (None, _) => None
		case (_, None) => None
		case (Some(aa), Some(bb)) => Some(f(aa,bb))
	}

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
		a flatMap (aa => b map (bb => f(aa,bb)))

	def map2FC[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
		for {
			aa <- a
			bb <- b
		} yield f(aa, bb)

	def Try[A](a: => A) = 
		try Some(a)
		catch { case e: Exception => None }

//	def parseInsuranceRateQuote(
//		age: String,
//		numberOfSpeedingTickets: String): Option[Double] = {
//		val optAge: Option[Int] = Try(age.toInt)
//		val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
//		//insuranceRateQuote(optAge, optTickets)
//		map2(optAge, optTickets)(insuranceRateQuote)
//	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
		case Nil => Some(Nil)
		case Cons(h,t) => h flatMap (hh => sequence(t) map (Cons(hh,_)))
	}

	def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
		case Nil => Some(Nil)
		case Cons(h,t) => map2(f(h), traverse(t)(f))(Cons(_,_))
	}

	def sequenceT[A](a: List[Option[A]]): Option[List[A]] = 
		traverse(a)(x => x)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
