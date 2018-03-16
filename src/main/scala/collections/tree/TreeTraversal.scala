package collections.tree

import scala.collection.immutable.Queue

/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
object TreeTraversal {

  /**
    * Depth First Search: Pre-Order node evaluation
    * @param current The current node in the traversal
    * @param evaluator The evaluator function; returns true if the node with the desired data is found
    * @tparam D The class of the data contained by the tree nodes
    * @return The first instance of T (data) found in a node for which evaluator(data) = true
    */
  def dfs[D](current: LazyTree[D])(evaluator: D => Boolean): Option[D] = {
    if(evaluator(current.data))
      return Some(current.data)
    val cs = current.generateChildren()
    for(child <- cs) {
      val data = dfs(child)(evaluator)
      if(data.nonEmpty) return data
    }
    None
  }

  /**
    * Breadth First Search
    * @param root The node from which the search will begin
    * @param evaluator The evaluator function; returns true if the node with the desired data is found
    * @tparam D The class of data contained by the tree nodes
    * @return The first instance of T (data) found in a node for which evaluator(data) = true
    */
  def bfs[D](root: LazyTree[D])(evaluator: D => Boolean): Option[D] = {
    bfsHelper(evaluator, Queue(root))
  }

  private def bfsHelper[D](evaluator: D => Boolean, queue: Queue[LazyTree[D]]): Option[D] = {
    if(queue.isEmpty) return None                         // If queue empty, we couldn't find it
    val (current, queueB) = queue.dequeue                 // Get next Node in queue
    if(evaluator(current.data)) return Some(current.data) // If this is what we're looking for, return it.
    val cs = current.generateChildren()                   // Otherwise generate children,
    val queueC = queueB ++ cs                             // add them to the queue,
    bfsHelper(evaluator, queueC)                          // and recurse
  }

}
