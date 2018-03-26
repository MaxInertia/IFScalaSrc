package experimental.neargen

import collections.tree.AbstractBenchmark
import org.scalameter.api._

import scala.util.Random

/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
object Nearest_Benchmark extends AbstractBenchmark {
  /* Inputs */
  val pow2sizes: Gen[Int] = Gen.exponential("numPoints")(2500, 10000, 2)
  //val sizes: Gen[Int] = Gen.range("numPoints")(10000, 20000, 10000)

  performance of "Points2D" in {
    /*measure method "divAndConqFASTER???" in {
      using(pow2sizes) in {r => PointCollection2D.nearestPair( setup2D(r) )}
      //using(sizes) in { r => daq2D(r)}
    }*/

    measure method "PC2D Divide and Conquer Algorithm" in {
      using(pow2sizes) in {r => PointCollection2D.nearestPair( setup2D2(r) )}
      //using(sizes) in { r => daq2D(r)}
    }

    measure method "bruteForce" in {
      using(pow2sizes) in {r => PointCollection.naiveNearestPair( setup2D(r) )}
      //using(sizes) in { r => brf2D(r) }
    }
  } // eo-2D-perf

  /*performance of "Points3D" in {
    measure method "divAndConq" in {
      using(pow2sizes) in {r =>     PointCollection.nearestPair( setup3D(r) )}
      //using(sizes) in { r => daq3D(r) }
    }

    measure method "bruteForce" in {
      using(pow2sizes) in { r => PointCollection.naiveNearestPair( setup3D(r) ) }
      //using(sizes) in { r => brf3D(r) }
    }
  } // eo-3D-perf*/


  /*def setup3D(n: Int): PointCollection[Point3D] = {
    val P = Array.fill(n) {Point3D(
        Random.nextDouble * 100,
        Random.nextDouble * 100,
        Random.nextDouble * 100)}
    PointCollection(P)
  }*/

  def setup2D(n: Int): PointCollection[Point2D] = {
    val P = Array.fill(n) {Point2D(
      Random.nextDouble * 100,
      Random.nextDouble * 100)}
    PointCollection(P)
  }

  def setup2D2(n: Int): PointCollection2D = {
    val P = Array.fill(n) {Point2D(
      Random.nextDouble * 100,
      Random.nextDouble * 100)}
    PointCollection2D(P)
  }
}
