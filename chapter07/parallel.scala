import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, ExecutorService}

// end goal: map function to list in parallel
// val outputList = parMap(inputList)(f)

def sum(ints: Seq[Int]): Int = 
	ints.foldLeft(0)((a,b) => a+b)

def sum(ints: IndexedSeq[Int]): Int = // Par[Int] =
	if (ints.size <= 1)
		ints.headOption getOrElse 0 // Par.unit(ints.headOption getOrElse 0)
	else { 
		val (l,r) = ints.splitAt(ints.length/2) 
		sum(l) + sum(r) // Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_+_)
	}

trait Par[+A]
object Par {
	type Par[A] = ExecutorService => Future[A]
	
	def unit[A](a: => A): Par[A] = 
		(es: ExecutorService) => new Future[A] {
			override def apply(cb: A => Unit): Unit =
				cb(a)
		}
	def run[A](es: ExecutorService)(p: Par[A]): A = {
		val ref = new AtomicReference[A]
		val latch = new CountDownLatch(1)
		p(es) { a => ref.set(a); latch.countDown }
		latch.await
		ref.get
	}
	// fork deadlocks on any fixed thread pool of size numforks-1 or fewer
	def fork[A](a: => Par[A]): Par[A] = 
		es => new Future[A] {
			override def apply(cb: A => Unit): Unit = 
				eval(es)(a(es)(cb))
		}
	def eval(es: ExecutorService)(r: => Unit): Unit = 
		es.submit(new Callable[Unit] { def call = r })
	def delay[A](fa: => Par[A]): Par[A] = 
		es => fa(es)
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
	def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
	def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
		(es: ExecutorService) => {
			val (af, bf) = (a(es), b(es))
			Map2Future(af, bf, f)
		}
	
	def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean = 
		p1(e).get == p2(e).get
	
	def sortPar(pa: Par[List[Int]]): Par[List[Int]] = 
		map(pa)(_.sorted)
	def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
		map2(pa, unit(()))((a,_) => f(a))
	
	def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
		val fbs: List[Par[B]] = ps.map(asyncF(f))
		sequence(fbs)
	}
	def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
		ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_::_))
	
	def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
		val pas: List[Par[List[A]]] = as map (asyncF((a: A) => if (f(a)) List(a) else List()))
		map(sequence(pas))(a => a.flatten)
	}
	
	def parWordCount(ss: List[String]): Par[Int] = {
		val pl: List[Par[Int]] = ss map (asyncF((s) => s.split(Array(' ','\t')).length))
		map(sequence(pl))(_.foldLeft(0)((a,b) => a+b))
	}
	
	private case class UnitFuture[A](newGet: A) extends Future[A] {
		override def get = newGet
		override def isDone = true
		override def get(timeout: Long, units: TimeUnit) = get
		override def isCancelled = false
		override def cancel(evenIfRunning: Boolean): Boolean = false
	}
	
	case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
		var cache: Option[C] = None
		override def isDone = cache.isDefined
		override def isCancelled = a.isCancelled || b.isCancelled
		override def cancel(evenIfRunning: Boolean) = 
			a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
		override def get = compute(Long.MaxValue)
		override def get(timeout: Long, units: TimeUnit): C = 
			compute(TimeUnit.MILLISECONDS.convert(timeout, units))
		
		private def compute(timeoutMs: Long): C = cache match {
			case Some(c) => c
			case None => {
				val start = System.currentTimeMillis
				val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
				val stop = System.currentTimeMillis
				val at = stop-start
				val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
				cache = Some(f(ar,br))
				cache.get
			}
		}
	}
}

// imported from java.util.concurrent
// abstract class ExecutorService {
	// def submit[A](a: Callable[A]): Future[A]
// }
// trait Callable[A] { def call: A }
sealed trait Future[A] {
	def get: A = ???
	def get(timeout: Long, unit: TimeUnit): A = ???
	def cancel(evenIfRunning: Boolean): Boolean = ???
	def isDone: Boolean = ???
	def isCancelled: Boolean = ???
	
	// wants to be private[parallelism]
	def apply(k: A => Unit): Unit = ???
}

Par.map2(Par.unit(1), Par.unit(1))(_+_)
