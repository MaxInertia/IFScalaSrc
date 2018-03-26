package experimental.nearest

/**
  * Created by Dorian Thiessen on 2018-03-25.
  */
sealed trait NTree {
  def update(other: Point2D, dist2other: Double): NTree
  def count: Int
  def toArray: Array[(Point2D, Double)]
}

case object Leaf extends NTree {
  // Base case: Replace Leaf with the Node
  def update(other: Point2D, dist2other: Double): NTree = Node(Leaf, other, dist2other, Leaf)
  def count = 0

  def toArray: Array[(Point2D, Double)] = Array[(Point2D, Double)]()
}

case class Node(var left: NTree = Leaf,
                var data: Point2D, var dist: Double,
                var right: NTree = Leaf) extends NTree {

  def update(other: Point2D, dist2other: Double): NTree = {
    if(dist2other == dist && other.x == data.x && other.y == data.y) return this
    else if(dist2other < dist) left = left.update(other, dist2other) // Update left side of tree
    else right = right.update(other, dist2other) // Update right side of tree
    this // Return updated tree
  }

  def count: Int = left.count + 1 + right.count

  def toArray: Array[(Point2D, Double)] =
    left.toArray ++ Array((data, dist)) ++ right.toArray

}