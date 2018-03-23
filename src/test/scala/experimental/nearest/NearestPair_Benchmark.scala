package experimental.nearest

import collections.tree.{AbstractBenchmark, IntTree}
import org.scalameter.api._

import scala.util.Random

/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
object NearestPair_Benchmark extends AbstractBenchmark {
  /* Inputs */
  //val sizes: Gen[Int] = Gen.range("nodes")(1, 100, 1)
  val e_sizes: Gen[Int] = Gen.exponential("numPoints")(100, 10000, 10)
  val sizes: Gen[Int] = Gen.range("numPoints")(10000, 40000, 10000)

  performance of "Points" in {

    /*measure method "naiveBruteForce" in {
      using(e_sizes) in {
        r => {
          //TODO: Factor out point creation
          val P = Array.fill(r){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
          NaiveBrute.closestPair1(P)
        }
      }
    }*/

    measure method "bruteForce" in {
      using(sizes) in {
        r => {
          //TODO: Factor out point creation
          val P = Array.fill(r){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
          NaiveBrute.closestPair2(P)
        }
      }
    }

    measure method "divAndConq" in {
      using(sizes) in {
        r => {
          //TODO: Factor out point creation
          val P = Array.fill(r){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
          Points(P).closestPair()
        }
      }
    }

  } // eo-performance
}
