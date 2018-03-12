package operations.search

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import collections.tree.{IntTree, LazyTree}
import operations.search.RangeBenchmark.using

/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
object Minimax_Alphabeta_Benchmark extends AbstractBenchmark {
  /* Inputs */
  val sizes: Gen[Int] = Gen.range("nodes")(1, 100, 1)
  val esizes: Gen[Int] = Gen.exponential("nodes")(1, 128, 2)

  performance of "TreeTraversal" in {
    measure method "minimax" in {
      using(esizes) in {
        r => TreeTraversal.minimax(IntTree.generate(r), 4)(evaluator2, isTerminal)
      }
    }
    measure method "alphabeta" in {
      using(esizes) in {
        r => TreeTraversal.alphabeta(evaluator2, isTerminal)(IntTree.generate(r), 4, maxFirst = true)
      }
    }
  }

  val eval: Int => Boolean = (x: Int) => false

  val evaluator: Int => Double = (x: Int) => x.toDouble
  val isTerminal: Int => Boolean = _ => false

  val evaluator2: Int => Double = (x: Int) => {
    print(x + ", ")
    x.toDouble
  }

}
