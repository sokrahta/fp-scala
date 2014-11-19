import java.util.concurrent._

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
	
	def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)
	def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
	def fork[A](a: => Par[A]): Par[A] = 
		es => es.submit(new Callable[A] {
			def call = a(es).get
		})
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
	def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
		(es: ExecutorService) => {
			val af = a(es)
			val bf = b(es)
			UnitFuture(f(af.get, bf.get))
		}
	
	private case class UnitFuture[A](get: A) extends Future[A] {
		def isDone = true
		def get(timeout: Long, units: TimeUnit) = get
		def isCancelled = false
		def cancel(evenIfRunning: Boolean): Boolean = false
	}
}

abstract class ExecutorService {
	def submit[A](a: Callable[A]): Future[A]
}
trait Callable[A] { def call: A }
trait Future[A] {
	def get: A
	def get(timeout: Long, unit: TimeUnit): A
	def cancel(evenIfRunning: Boolean): Boolean
	def isDone: Boolean
	def isCancelled: Boolean
}

Par.map2(Par.unit(1), Par.unit(1))(_+_)
