package experimental.neargen

/**
  * Created by Dorian Thiessen on 2018-03-25.
  */
case class PointCollection3D(points: Array[Point3D]) extends PointCollection[Point3D] {
  private var sorted: Boolean = false
  private var xSorted: Array[Point3D] = _
  private var ySorted: Array[Point3D] = _
  private var zSorted: Array[Point3D] = _

  private val n = points.length
  override def size: Int = n

  def xS = xSorted
  def yS = ySorted
  def zS = zSorted

  def allSort(): Unit = {
    if(!sorted) {
      xSorted = points.sortWith(_.x < _.x)
      ySorted = points.sortWith(_.y < _.y)
      zSorted = points.sortWith(_.z < _.z)
      sorted = true
    }
  }

  def split(): (PointCollection[Point3D], PointCollection[Point3D]) = {
    val half = xSorted.length/2

    val Qx = Array.fill[Point3D](half){null}
    var Qy = Array[Point3D]()
    var Qz = Array[Point3D]()

    val Rx = Array.fill[Point3D](xSorted.length - half){null}
    var Ry = Array[Point3D]()
    var Rz = Array[Point3D]()

    // X's: Qx and Rx
    for(i <- 0 until half) {
      Qx(i) = xSorted(i)
    }
    for(i <- half until xSorted.length) {
      Rx(i - half) = xSorted(i)
    }

    // Y's: Qy and Ry
    //for(p <- Px) assert(!p.halfseen)
    //assert(Py.length == Pz.length)

    // TODO: Could be simplified (with no change negative effect on performance?) by breaking into 2 independent for loops
    // TODO: PRIORITY: Then halfSeen is not required making it easier to generalize to higher dimensions

    for(i <- ySorted.indices) {
      val py = ySorted(i)
      if(ySorted(i).xsIndex < half) {
        py.ysIndex = Qy.length + 1
        Qy = Qy :+ py
      } else {
        py.ysIndex = Ry.length + 1
        Ry = Ry :+ py
      }
      if(py.halfSeen && py.xsIndex >= half) {
        py.xsIndex = py.xsIndex - half
        py.halfSeen = false
      } else py.halfSeen = true

      // Z
      val pz = zSorted(i)
      if(zSorted(i).xsIndex < half) {
        pz.zsIndex = Qz.length + 1
        Qz = Qz :+ pz
      } else {
        pz.zsIndex = Rz.length + 1
        Rz = Rz :+ pz
      }
      if(pz.halfSeen && pz.xsIndex >= half) {
        pz.xsIndex = pz.xsIndex - half
        pz.halfSeen = false
      } else pz.halfSeen = true
    }
    for(p <- zSorted) p.halfSeen = false // TODO: Shouldn't need this, why aren't all set to false?

    // Put each of the two halves in a point collection and return
    val leftP = PointCollection3D(Qx)
    leftP.xSorted = Qx
    leftP.ySorted = Qy
    leftP.zSorted = Qz
    val rightP = PointCollection3D(Rx)
    rightP.xSorted = Rx
    rightP.ySorted = Ry
    rightP.zSorted = Rz
    (leftP, rightP)
  }

  /*override def baseCase(): (Point3D, Point3D) = {
    if(ps.length == 3) {
      val d01 = ps(0).distTo(ps(1))
      val d02 = ps(0).distTo(ps(2))
      val d12 = ps(1).distTo(ps(2))
      if(d01 < d02) {
        if(d01 < d12) (ps(0), ps(1))
        else (ps(1), ps(2))
      } else {
        if(d02 < d12) (ps(0), ps(2))
        else (ps(1), ps(2))
      }
    } else if(ps.length == 2) (ps(0), ps(1))
    else {
      assert(assertion = false, "IMPLEMENT Brute Nearest beyond 3!") // TODO: Do this =)
      (ps(0), ps(0))
    }
  }*/

  def maxXCoordinate(): Double = xSorted.last.x

  override def nearestWithin(sep: Double, del: Double): Option[(Point3D, Point3D)] = {
    // TODO: Optimize by Q.reverse.takewhile and R.takewhile
    val S = ySorted.filter(p => {math.abs(p.x - sep) <= del})
    if(S.length < 3) return None
    // TODO: Optimize by not comparing pairs of points from the same recursive call (Q's and R's)
    var bestDist = Double.MaxValue
    var (bestI, bestJ) = (-1, -1)
    for(i <- S.indices) {
      for(j <- (i+1) until i+17) { // Max 16 iterations
        if(i != j && j < S.length) {
          val dist = S(i).distTo(S(j))
          if (dist < bestDist) {
            bestDist = dist
            bestI = i
            bestJ = j
          }
        }
      }
    }
    Some( (S(bestI), S(bestJ)) )
  }

  override protected def ps: Array[Point3D] = points
}

/*object PointCollection3D {
  def apply(points: Array[Point3D]): PointCollection3D = new PointCollection3D(points)
}*/