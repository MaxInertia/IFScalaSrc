package experimental.neargen

/**
  * Created by Dorian Thiessen on 2018-03-24.
  */
trait PointCollection[P <: Point] {

  def size: Int

  /**
    * Sort the collection once along each axi, each sort can be independent.
    * This is required for iterating over points sorted by any coordinate.
    */
  def allSort(): Unit

  /**
    * Split this points collection into two.
    * The size of the two is equal within 1.
    * The first contains the half of the points with the smallest first coordinate.
    * @return The first half and the second half of this Point Collection.
    */
  def split(): (PointCollection[P], PointCollection[P])

  /**
    * Finds the closest pair of points out of the subset of
    * points within 'del' of the line/plane/region 'x = sep'
    * @param sep position of separation line
    * @param del the max distance from sep to accept points
    * @return Closest pair of points
    */
  def nearestWithin(sep: Double, del: Double): Option[(P, P)]

  def baseCase(): (P, P) = {
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
  }

  def maxXCoordinate(): Double

  protected def ps: Array[P]
  protected def xS: Array[P]
}

object PointCollection {
  def apply(ps: Array[Point2D]): PointCollection[Point2D] = PointCollection2D(ps)
  def apply(ps: Array[Point3D]): PointCollection[Point3D] = PointCollection3D(ps)

  /**
    * Finds the nearest pair of points in the collection
    * @param pc        The point collection
    * @param threshold The number of points at which brute force comparisons will be applied
    *                  Minimum: 3 (Default)
    * @tparam P The type of point (dimensions are unknown here)
    * @return The pair of points separated by the smallest distance
    */
  def nearestPair[P <: Point](pc: PointCollection[P], threshold: Int = 3): (P, P) = {
    require(threshold >= 3)

    def min(a: Double, b: Double) = if (a < b) a else b

    def loop(pc: PointCollection[P]): (P, P) = {
      if (pc.size <= threshold) return pc.baseCase()

      val (pcL, pcR) = pc.split()
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

  def naiveNearestPair[P <: Point](pc: PointCollection[P]): (P, P) = {
    val points = pc.ps
    var bp1: P = points(0)
    var bp2: P = points(1)
    var smallestDistance = Double.MaxValue
    for {
      i <- points.indices
      j <- i + 1 until points.length
    } {
      val dist = points(i).distTo(points(j))
      if (dist < smallestDistance) {
        smallestDistance = dist
        bp1 = points(i)
        bp2 = points(j)
      }
    }
    (bp1, bp2)
  }

  /*
  def naiveNearestPair2D(pc: PointCollection[Point2D]): (Point2D, Point2D) = {
    val points = pc.ps
    var bestI = -1
    var bestJ = -1
    var bestDist = Double.MaxValue
    for {
      i <- points.indices
      j <- i + 1 until points.length
    } {
      val dist = points(i).distTo(points(j))
      if (dist < bestDist) {
        bestDist = dist
        bestI = i
        bestJ = j
      }
    }
    (points(bestI), points(bestJ))
  }

  def naiveNearestPair3D(pc: PointCollection[Point3D]): (Point3D, Point3D) = {
    val points = pc.ps
    var bestI = -1
    var bestJ = -1
    var bestDist = Double.MaxValue
    for {
      i <- points.indices
      j <- i + 1 until points.length
    } {
      val dist = points(i).distTo(points(j))
      if (dist < bestDist) {
        bestDist = dist
        bestI = i
        bestJ = j
      }
    }
    (points(bestI), points(bestJ))
  }
  */

}