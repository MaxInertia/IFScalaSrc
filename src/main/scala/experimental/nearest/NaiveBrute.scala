package experimental.nearest

import experimental.NearestKPoints.{Point2D, d}

object NaiveBrute {
  def closestPair(P: Array[Point2D]): (Point2D, Point2D, Double) = {
    // TODO: Implement Brute-Force Closest-Pair solver, use to verify clever solution
    var (bp1, bp2): (Point2D, Point2D) = (null, null)
    var smallestDistance = Double.MaxValue
    for{
      i <- P.indices
      j <- P.indices
      if i != j
    } {
      val dist = P(i).distTo(P(j))
      if(dist < smallestDistance) {
        smallestDistance = dist
        bp1 = P(i)
        bp2 = P(j)
      }
    }
    (bp1, bp2, smallestDistance)
  }
}
