package experimental.nearest

import collections.tree.{AbstractBenchmark, IntTree}
import org.scalameter.api._

/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
object NearestPair_Benchmark extends AbstractBenchmark {
  /* Inputs */
  //val sizes: Gen[Int] = Gen.range("nodes")(1, 100, 1)
  val e_sizes: Gen[Int] = Gen.exponential("nodes")(6, 128, 2)
  val indices: Gen[Int] = Gen.range("indices")(1, 10, 1)

  performance of "Points" in {

    /*using(indices) in {
      i =>
    }*/

    measure method "bruteForce" in {
      using(e_sizes) in {
        r => {

        }
      }
    }
    measure method "alphaBeta" in {
      using(e_sizes) in {
        r => {

        }
      }
    }
  }


}
