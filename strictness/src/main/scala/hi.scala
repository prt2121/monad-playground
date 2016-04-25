sealed trait Stream[+A] {

  import Stream.cons
  import collection.mutable.ListBuffer

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // EXERCISE 5.2
  // Write the function take(n)
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Write the function takeWhile for returning all starting elements of a Stream
  // that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  // Write a function to convert a Stream to a List
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailRec: List[A] = {
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }
}

case object Empty extends Stream[Nothing]

// thunks: () => A, () => Stream[A]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[B](hd: => B, tl: => Stream[B]): Stream[B] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
}

object Hi {
  def main(args: Array[String]) = println("hi!")
}