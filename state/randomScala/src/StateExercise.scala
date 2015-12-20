import scala.collection.mutable.ListBuffer

/**
  * Created by pt2121 on 12/17/15.
  */
object StateExercise {
  // ex 6.1 page 82
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -1 * (n + 1) else n, r)
  }

  // ex 6.2 page 83
  def double(rng: RNG): (Double, RNG) = {
    val (n1, r1) = nonNegativeInt(rng)
    val n = n1 / (Int.MaxValue.toDouble + 1)
    (n, r1)
  }

  // ex 6.3 page 83
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, r1) = rng.nextInt
    val (n2, r2) = double(r1)
    ((n1, n2), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (n1, r1) = rng.nextInt
    val (n2, r2) = double(r1)
    ((n2, n1), r2)
  }

  // ex 6.3 page 83
  // Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var r = rng
    var l = ListBuffer[Int]()
    1 to count foreach { i =>
      val (n, gen) = r.nextInt
      l += n
      r = gen
    }
    (l.toList, r)
  }

  // ex 6.3 page 83
  def intsRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (List(), rng)
    else {
      val (n1, r1) = rng.nextInt
      val (n2, r2) = intsRecursive(count - 1)(r1)
      (n1 :: n2, r2)
    }
  }

}
