package experimental

import experimental.AdvancedFP.Point2D

/**
  * Created by Dorian Thiessen on 2018-03-21.
  */
object AdvancedFP extends App {
  type Point2D = (Int, Int)

  val ps = List(
    (0,0),
    (1,1),
    (2,8))
  println(ps)

  val rectangle = Rectangle(ps.head, 7, 22)
  println(rectangle)

  val list = List((x:Int) => x*x)
  println(list)
}

sealed trait Shape {
  def width: Int
  def height: Int
}

case class Rectangle(corner: Point2D, width: Int, height: Int) extends Shape

case class Composite[F[_], G[_], A](run: F[G[A]])
