/**
  * Created by pt2121 on 12/17/15.
  */
trait RNG {
  def nextInt: (Int, RNG)
}