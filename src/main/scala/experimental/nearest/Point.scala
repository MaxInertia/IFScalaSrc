package experimental.nearest

trait Point

case class Point2D(x: Double, y: Double) {
  var xsIndex: Int = -1
  var ysIndex: Int = -1
  def distTo(other: Point2D): Double = math.sqrt(
    math.pow(x - other.x, 2) + math.pow(y - other.y, 2)
  )

  override def toString: String = s"($x, $y)"
}

// Stack of things to be proved: Resolvent
// Initially: Resolvent is the Query
// Resolution: Modus Ponens