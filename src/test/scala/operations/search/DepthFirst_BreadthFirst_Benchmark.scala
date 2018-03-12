package operations.search

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import collections.tree.IntTree

/**
  * Created by Dorian Thiessen on 2018-03-12.
  */
object DepthFirst_BreadthFirst_Benchmark extends AbstractBenchmark {
  /* Inputs */
  val sizes: Gen[Int] = Gen.exponential("nodes")(1, 128, 2)

  performance of "TreeTraversal" in {
    measure method "bfs" in {
      using(sizes) in {
        r => TreeTraversal.bfs(IntTree.generate(r))(eval)
      }
    }
    measure method "dfs" in {
      using(sizes) in {
        r => TreeTraversal.dfs(IntTree.generate(r))(eval)
      }
    }
  }

  val eval: Int => Boolean = (x: Int) => false

  val evaluator: Int => Double = (x: Int) => x.toDouble
  val isTerminal: Int => Boolean = _ => false
}
