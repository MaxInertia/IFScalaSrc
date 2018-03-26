package experimental.nearest

trait Point

case class Point2D(x: Double, y: Double) {
  var xsIndex: Int = -1
  var ysIndex: Int = -1

  var nearest: NTree = Leaf

  def setNear(other: Point2D, dist: Double): Unit = {
    nearest = nearest.update(other, dist) // TODO: If too many are added, cut off furthest?
  }

  def distTo(other: Point2D): Double = math.sqrt(
    math.pow(x - other.x, 2) + math.pow(y - other.y, 2)
  )

  override def toString: String = s"($x, $y)"
}

case class Point3D(x: Double, y: Double, z: Double) {
  var xsIndex: Int = -1
  var ysIndex: Int = -1
  var zsIndex: Int = -1
  //var halfseen: Boolean = false

  private var nearest: Point3D = _
  private var distance2Nearest: Double = Double.MaxValue

  def setNearest(other: Point3D): Unit = {
    if(nearest != null) {
      nearest.nearest = null
      nearest.distance2Nearest = Double.MaxValue
    }
    nearest = other
    distance2Nearest = distTo(other)
    other.nearest = this
    other.distance2Nearest = distance2Nearest
  }

  def getNearest: (Point3D, Double) = (nearest, distance2Nearest)

  def distTo(other: Point3D): Double = math.sqrt(
    math.pow(x - other.x, 2) + math.pow(y - other.y, 2) + math.pow(z - other.z, 2)
  )

  override def toString: String = s"($x, $y, $z)"
}

// Stack of things to be proved: Resolvent
// Initially: Resolvent is the Query
// Resolution: Modus Ponens