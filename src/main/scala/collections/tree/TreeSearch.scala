package collections.tree

/**
  * Created by Dorian Thiessen on 2018-03-10.
  *
  * N: Node class
  */
/*trait TreeSearch[N] {
  def search(): Option[N]
}*/

object TreeSearch {
  def miniMax[S](utilityFn: S => Double, isTerminal: S => Boolean, counter: ()=>Unit = ()=>())
                (root: LazyTree[S], depthLimit: Int): Double = {
    def search(current: LazyTree[S], depth: Int = 0, isMax: Boolean): Double = {
      counter()

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

  def alphaBeta[S](utilityFn: S => Double, isTerminal: S => Boolean, counter: ()=>Unit = ()=>())
                  (root: LazyTree[S], depthLimit: Int, maxFirst: Boolean = true): Double = {

    //var alpha = InitAlpha
    //var beta = InitBeta

    def search(current: LazyTree[S], depth: Int = 0, alpha: Double, beta: Double, isMax: Boolean): Double = {
      counter()

      if(isTerminal(current.data) || depth==0)
        return utilityFn(current.data)

      if(isMax) { // Maximizing player
        var bestValue = -Double.MaxValue
        val possibleStates = current.generateChildren()
        for(child <- possibleStates) {
          val v = search(child, depth - 1, alpha = bestValue, beta, isMax = false)
          //val v = search(child, depth - 1, isMax = false)
          if(v >= beta) return beta // Fail hard beta-cutoff
          if(v > bestValue) {
            bestValue = v
            //alpha = bestValue
          }
          bestValue = math.max(bestValue, v)
        }
        bestValue

      } else { // Minimizing player
        var bestValue = Double.MaxValue
        val possibleStates = current.generateChildren()
        for (child <- possibleStates) {
          val v = search(child, depth - 1, alpha, beta = bestValue, isMax = true)
          //val v = search(child, depth - 1, isMax = true)
          if (v <= alpha) return alpha // Fail hard alpha-cutoff
          if (v < bestValue) {
            bestValue = v
            //beta = bestValue
          }
          bestValue = math.max(bestValue, v)
        }
        bestValue
      }

    }

    search(root, depthLimit, InitAlpha, InitBeta, isMax = maxFirst)
  }

  def negamaxAlphaBeta[S](utilityFn: S => Double,
                          isTerminal: S => Boolean,
                          counter: ()=>Unit = ()=>()): (LazyTree[S], Int, Int, Double, Double) => Double = {

    def search (current: LazyTree[S],
                depth: Int,
                color: Int,
                alpha: Double = -Double.MaxValue,
                beta: Double = Double.MaxValue): Double = {
      counter()

      if(isTerminal(current.data) || depth==0)
        return color * utilityFn(current.data)

      val children = current.generateChildren()  // TODO: Enable ordering of successors

      var bestValue: Double = alpha
      for(child <- children) {
        val v = -search(child, depth - 1, -color, -beta, -bestValue)
        bestValue = math.max(bestValue, v)
        if(bestValue >= beta) {
          return bestValue
        }
      }
      bestValue
    }

    search
  }
}