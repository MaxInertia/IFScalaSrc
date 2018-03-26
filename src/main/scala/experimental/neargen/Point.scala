package experimental.neargen

/**
  * Created by Dorian Thiessen on 2018-03-24.
  */
trait Point {
  /**
    * Coordinates of the point returned as an array.
    * An N-dimensional point will contain an array of size N
    * @return Array of values for each coordinate
    */
  //def coords: Array[Double]

  /**
    * The distance between the two points in space with dimension count
    * equal to the minimum of the two input points.
    *
    * Probably not the most efficient solution,
    * just the simplest for abstracting over point dimensionality.
    *
    * If dimensionality differs, lowest is used.
    * It does not imply higher dimensionality with zero value for those coordinates.
    *
    * @param other The other point
    * @return The distance between the two points.
    */
  def distTo(other: Point): Double

  // Turns out the slowdown from the below method is insane, roughly 30x slower.
  /*def distTo(other: Point): Double = math.sqrt(
    coords.zip(other.coords)
      .map(v => math.pow(v._1 - v._2, 2))
      .sum)*/
}

// NO obvious performance change when using typeclass here
/*trait Locality[P] {
  def separation(p1: P, p2: P): Double
}

object IsPoint {
  implicit object P2D extends Locality[Point2D] {
    def separation(p1: Point2D, p2: Point2D): Double = math.sqrt(
      math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2)
    )
  }
}*/

case class Point2D(x: Double, y: Double) extends Point {
  def coords: Array[Double] = Array(x, y)
  var xsIndex: Double = _
  var ysIndex: Double = _

  def distTo(other: Point): Double = {
    val p2 = other.asInstanceOf[Point2D]
    math.sqrt(math.pow(x - p2.x, 2) + math.pow(y - p2.y, 2))
  }
  /*// Typeclass alternative
  def distTo(other: Point): Double = {
    import IsPoint._
    val ops = implicitly[Locality[Point2D]]
    ops.separation(this, other.asInstanceOf[Point2D])
  }*/
}

case class Point3D(x: Double, y: Double, z: Double) extends Point {
  def coords: Array[Double] = Array(x, y, z)
  var xsIndex: Double = _
  var ysIndex: Double = _
  var zsIndex: Double = _
  var halfSeen: Boolean = false

  override def distTo(other: Point): Double = {
    val p2 = other.asInstanceOf[Point3D]
    math.sqrt(math.pow(x - p2.x, 2) + math.pow(y - p2.y, 2) + math.pow(z - p2.z, 2))
  }
}