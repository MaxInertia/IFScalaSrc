package experimental.nearest

import experimental.NearestKPoints.{Point2D, d}

object NaiveBrute {
  def closestPair1(P: Array[Point2D]): (Point2D, Point2D, Double) = {
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

  def closestPair2(P: Array[Point2D]): (Point2D, Point2D, Double) = {
    var (bp1, bp2): (Point2D, Point2D) = (null, null)
    var smallestDistance = Double.MaxValue
    for{
      i <- P.indices
      j <- i+1 until P.length
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
