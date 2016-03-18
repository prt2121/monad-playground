import cats._
import cats.syntax.eq._

import simulacrum._

sealed trait TrafficLight

object TrafficLight {
  def red: TrafficLight = Red

  def yellow: TrafficLight = Yellow

  def green: TrafficLight = Green

  case object Red extends TrafficLight

  case object Yellow extends TrafficLight

  case object Green extends TrafficLight

  implicit val trafficLightEq: Eq[TrafficLight] =
    new Eq[TrafficLight] {
      def eqv(a1: TrafficLight, a2: TrafficLight): Boolean = a1 == a2
    }
}

// generate typeclass using simulacrum
@typeclass trait YesNo[A] {
  self =>
  def yesNo(a: A): Boolean
}

object YesNo {
  def fromYesNo[A](f: A => Boolean): YesNo[A] = new YesNo[A] {
    def yesNo(a: A): Boolean = f(a)
  }
}

object MyCat {

  import YesNo.ops._

  def main(args: Array[String]): Unit = {

    implicit val stringYesNo: YesNo[String] = YesNo.fromYesNo({
      case "Yes" | "yes" | "YES" => true
      case _ => false
    })

    println(TrafficLight.red === TrafficLight.yellow)
    println(TrafficLight.red === TrafficLight.red)

    println("Yes".yesNo)
    println("yes".yesNo)
    println("YES".yesNo)
    println("wat".yesNo)
  }
}