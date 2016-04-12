import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

class Parallel {
  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a, b) => a + b)

  //  def sum(ints: IndexedSeq[Int]): Par[Int] =
  //    if (ints.size <= 1)
  //      Par.unit(ints.headOption getOrElse 0)
  //    else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  //    }

  object Par {

    // p. 105. a function
    type Par[A] = ExecutorService => Future[A]

    // extracting the result
    // def get[A](a: Par[A]): A
    // let’s rename our get function to run,
    // and dictate that this is where the parallelism actually gets implemented
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    // a unit of parallelism
    // def unit[A](a: => A): Par[A] = ???
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    // indicate that Par should be run in a separate logical thread
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    // EXERCISE 7.4
    // convert any function A => B to one that evaluates its result asynchronously
    def asyncF[A, B](f: A => B): A => Par[B] =
      f _ andThen lazyUnit _

    // implicit conversions for infix syntax
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A])

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

  }

}
