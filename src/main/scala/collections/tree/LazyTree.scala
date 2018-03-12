package collections.tree

/** Implementations of LazyTree are Trees that are not required to explicitly define their children,
  * instead provide a way of generating those children on demand.
  *
  * It can be thought of as an implicit tree, where the only explicitly defined node is an instance
  * of the class extending this trait.
  *
  * D: Data stored in nodes, each instance of LazyTree is a node and a tree.
  *
  * Created by Dorian Thiessen on 2018-03-10.
  */
trait LazyTree[D] {
  def data: D
  def generateChildren(): Seq[LazyTree[D]]
}
