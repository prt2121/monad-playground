/**
  * Created by pt2121 on 12/19/15.
  *
  * computation that carries some state along, or state action, state transition, or even statement
  */
// type State[S, +A] = S => (A, S)
// type Rand[+A] = RNG => (A, RNG)
case class State[S, +A](run: S => (A, S)) {

  def unit[T, B](a: B): State[T, B] = State { (s: T) =>
    (a, s)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { (s: B) =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }

  def map[B](f: A => B): State[S, B] = flatMap {
    a => unit(f(a))
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap {
    a => sb.map(b => f(a, b))
  }

}

