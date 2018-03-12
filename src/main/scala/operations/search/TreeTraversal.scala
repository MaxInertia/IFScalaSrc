package operations.search

import collections.tree.LazyTree

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


  def minimax[S](root: LazyTree[S], depthLimit: Int)
                (utilityFn: S => Double, isTerminal: S => Boolean): Double = {
    def search(current: LazyTree[S], depth: Int = 0, isMax: Boolean): Double = {
      if(isTerminal(current.data) || depth==0)
        return utilityFn(current.data)

      if(isMax) {
        var bestValue = -Double.MaxValue
        val possibleStates = current.generateChildren()
        for(child <- possibleStates) {
          val v = search(child, depth-1, isMax = !isMax)
          bestValue = math.max(bestValue, v)
        }
        bestValue
      } else {
        var bestValue = Double.MaxValue
        val possibleStates = current.generateChildren()
        for(child <- possibleStates) {
          val v = search(child, depth-1, isMax = !isMax)
          bestValue = math.min(bestValue, v)
        }
        bestValue
      }
    }

    search(root, depthLimit, isMax = true)
  }


  private val InitAlpha = -Double.MaxValue
  private val InitBeta  = Double.MaxValue

  def alphabeta[S](utilityFn: S => Double, isTerminal: S => Boolean)
                  (root: LazyTree[S], depthLimit: Int, maxFirst: Boolean = true): Double = {

    def search(current: LazyTree[S], depth: Int = 0, alpha: Double, beta: Double, isMax: Boolean): Double = {

      if(isTerminal(current.data) || depth==0)
        return utilityFn(current.data)

      if(isMax) {
        var bestValue = alpha
        val possibleStates = current.generateChildren()
        for(child <- possibleStates) {
          val v = search(child, depth-1, alpha = bestValue, beta, isMax = false)
          if(v >= beta) return beta // Fail hard beta-cutoff
          if(v > bestValue) bestValue = v
          bestValue = math.max(bestValue, v)
        }
        bestValue
      } else {
        var bestValue = beta
        val possibleStates = current.generateChildren()
        for (child <- possibleStates) {
          val v = search(child, depth - 1, alpha, beta = bestValue, isMax = true)
          if (v <= alpha) return alpha // Fail hard alpha-cutoff
          if (v < bestValue) bestValue = v
          bestValue = math.max(bestValue, v)
        }
        bestValue
      }
    }
    search(root, depthLimit, InitAlpha, InitBeta, isMax = maxFirst)
  }
}
