package collections.tree

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import collections.tree.{IntTree, LazyTree}

/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
object Minimax_Alphabeta_Benchmark extends AbstractBenchmark {
  /* Inputs */
  //val sizes: Gen[Int] = Gen.range("nodes")(1, 100, 1)
  val e_sizes: Gen[Int] = Gen.exponential("nodes")(6, 128, 2)

  performance of "TreeTraversal" in {
    measure method "miniMax" in {
      using(e_sizes) in {
        r => {
          var count = 0
          val evaluatorCounter: Int => Double = (x: Int) => {
            count += 1
            x.toDouble
          }

          TreeSearch.miniMax(evaluatorCounter, isTerminal)(IntTree.generate(r), 4)
          println(s"\tSearched $count nodes")
        }
      }
    }
    measure method "alphaBeta" in {
      using(e_sizes) in {
        r => {
          var count = 0
          val evaluatorCounter: Int => Double = (x: Int) => {
            count+=1
            x.toDouble
          }

          TreeSearch.alphaBeta(evaluatorCounter, isTerminal)(IntTree.generate(r), 4, maxFirst = true)
          println(s"\tSearched $count nodes")
        }
      }
    }
  }

  val eval: Int => Boolean = (x: Int) => false

  val evaluator: Int => Double = (x: Int) => x.toDouble
  val isTerminal: Int => Boolean = _ => false

  val evaluator2: Int => Double = (x: Int) => {
    //print(x + ", ")
    x.toDouble
  }

}
