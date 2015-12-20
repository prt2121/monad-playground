/**
  * Created by pt2121 on 12/17/15.
  */
object RNG {

  type Rand[+A] = RNG => (A, RNG)
  // or type Rand[A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = rand(rng)
    (f(a), r)
  }

  def mapV2[A, B](rand: Rand[A])(f: A => B): Rand[B] = flatMap(rand) { x =>
    unit(f(x))
  }

  // ex 6.6 page 85
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a1, r1) = ra(rng)
    val (a2, r2) = rb(r1)
    (f(a1, a2), r2)
  }

  def map2V2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  // ex 6.8 page 87
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // ex 6.5 page 85
  def double: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): Rand[(Int, Double)] = both(int, double)

  def doubleInt(rng: RNG): Rand[(Double, Int)] = both(double, int)

  // % remainder
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  case class SimpleRng(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRng = SimpleRng(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRng)
    }
  }

}
