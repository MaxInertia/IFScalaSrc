package experimental.neargen

/**
  * Created by Dorian Thiessen on 2018-03-25.
  */
//TODO: Abstract out the type of container? "Array" in this case
case class PointCollection2D(points: Array[Point2D]) extends PointCollection[Point2D] {
  private var sorted: Boolean = false
  //TODO: What can be memoized to improve performance?
  private var xSorted: Array[Point2D] = _
  private var ySorted: Array[Point2D] = _

  //private val n = points.length
  override def size: Int = points.length

  def xS = xSorted
  def yS = ySorted

  def allSort(): Unit = {
    if(!sorted) {
      xSorted = points.sortWith(_.x <= _.x)
      ySorted = points.sortWith(_.y <= _.y)
      sorted = true
    }
  }

  def split(): (PointCollection[Point2D], PointCollection[Point2D]) = {
    // TODO: If the points collection (cur: Array) does not have  .length, this could be memoized
    val half = points.length/2

    var Qx = Array.fill[Point2D](half){null}
    var Qy = Array[Point2D]()
    var Rx = Array.fill[Point2D](points.length - half){null}
    var Ry = Array[Point2D]()

    // Split points up via x-coordinate: O(n)
    //  Qx = lower half
    //  Rx = upper half
    for(i <- 0 until half) {
      Qx(i) = xSorted(i)
    }
    for(i <- half until xSorted.length) {
      Rx(i - half) = xSorted(i)
    }

    // Distribute ySorted according to split above: O(n)
    // This retains all-sort on split (to avoid resorting)
    for(i <- ySorted.indices) {
      val p = ySorted(i)
      if(ySorted(i).xsIndex < half) {
        p.ysIndex = Qy.length + 1
        Qy = Qy :+ p
      } else {
        p.ysIndex = Ry.length + 1
        Ry = Ry :+ p
      }
      if(p.xsIndex >= half) p.xsIndex = p.xsIndex - half
    }

    // Put each of the two halves in a point collection and return
    val leftP = PointCollection2D(Qx)
    leftP.xSorted = Qx
    leftP.ySorted = Qy
    val rightP = PointCollection2D(Rx)
    rightP.xSorted = Rx
    rightP.ySorted = Ry
    (leftP, rightP)
  }

  def split2D(): (PointCollection2D, PointCollection2D) = {
    // TODO: If the points collection (cur: Array) does not have constant time .length, this could be memoized
    val half = points.length/2

    var Qx = Array.fill[Point2D](half){null}
    var Qy = Array[Point2D]()
    var Rx = Array.fill[Point2D](points.length - half){null}
    var Ry = Array[Point2D]()

    // Split points up via x-coordinate: O(n)
    //  Qx = lower half
    //  Rx = upper half
    for(i <- 0 until half) {
      Qx(i) = xSorted(i)
    }
    for(i <- half until xSorted.length) {
      Rx(i - half) = xSorted(i)
    }

    // Distribute ySorted according to split above: O(n)
    // This retains all-sort on split (to avoid resorting)
    for(i <- ySorted.indices) {
      val p = ySorted(i)
      if(ySorted(i).xsIndex < half) {
        p.ysIndex = Qy.length + 1
        Qy = Qy :+ p
      } else {
        p.ysIndex = Ry.length + 1
        Ry = Ry :+ p
      }
      if(p.xsIndex >= half) p.xsIndex = p.xsIndex - half
    }

    // Put each of the two halves in a point collection and return
    val leftP = PointCollection2D(Qx)
    leftP.xSorted = Qx
    leftP.ySorted = Qy
    val rightP = PointCollection2D(Rx)
    rightP.xSorted = Rx
    rightP.ySorted = Ry
    (leftP, rightP)
  }

  /*override def baseCase(): (Point2D, Point2D) = {
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

  override def nearestWithin(sep: Double, del: Double): Option[(Point2D, Point2D)] = {
    val S = ySorted.filter(p => {math.abs(p.x - sep) <= del})  // TODO: Optimize by Q.reverse.takewhile and R.takewhile?
    if(S.length < 3) return None
    var bestDist = Double.MaxValue
    var (bestI, bestJ) = (-1, -1)
    // TODO: Optimize by not comparing pairs of points from the same recursive call (Q's and R's)
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

  override protected def ps: Array[Point2D] = points
}

object PointCollection2D {
  def apply(points: Array[Point2D]): PointCollection2D = new PointCollection2D(points)

  def nearestPair(pc: PointCollection2D, threshold: Int = 3): (Point2D, Point2D) = {
    require(threshold >= 3)

    def min(a: Double, b: Double) = if (a < b) a else b

    def loop(pc: PointCollection2D): (Point2D, Point2D) = {
      if (pc.size <= threshold) return pc.baseCase()

      val (pcL, pcR) = pc.split2D()
      val (pL1, pL2) = loop(pcL)
      val (pR1, pR2) = loop(pcR)

      val delta = min(pL1.distTo(pL2), pR1.distTo(pR2))
      val sep = pcL.maxXCoordinate()
      val os = pc.nearestWithin(sep, delta)
      if (os.nonEmpty) {
        val (s1, s2) = os.get
        if (s1.distTo(s2) < delta) return (s1, s2)
      }

      if (pL1.distTo(pL2) < pR1.distTo(pR2)) (pL1, pL2)
      else (pR1, pR2)
    }

    pc.allSort()
    loop(pc)
  }

  /*
  implicit object PC2D extends PointCollection[Point2D] {
    /**
      * Sort the collection once along each axi, each sort can be independent.
      * This is required for iterating over points sorted by any coordinate.
      */
    override def allSort(): Unit = ???
  }*/
}