package experimental.nearest

import collections.tree.AbstractBenchmark
import org.scalameter.api._

import scala.util.Random

/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
object NearestPair_Benchmark extends AbstractBenchmark {
  /* Inputs */
  val e_sizes: Gen[Int] = Gen.exponential("numPoints")(2, 3000, 2)
  val pow2sizes: Gen[Int] = Gen.exponential("numPoints")(2500, 20000, 2)

  performance of "Points2D" in {
    measure method "bruteForce" in {
      using(e_sizes) in {
        r => {
          //TODO: Factor out point creation
          val P = Array.fill(r){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
          NaiveBrute.closestPair2D(P)
        }
      }
      using(pow2sizes) in {
        r => {
          //TODO: Factor out point creation
          val P = Array.fill(r){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
          NaiveBrute.closestPair2D(P)
        }
      }
    }
    measure method "divAndConq" in {
      using(pow2sizes) in {
        r => {
          //TODO: Factor out point creation
          val P = Array.fill(r){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
          Points2D(P).closestPair()
        }
      }
      using(e_sizes) in {
        r => {
          //TODO: Factor out point creation
          val P = Array.fill(r){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
          Points2D(P).closestPair()
        }
      }
    }
    /*
    ::Benchmark Points.betterBruteForce::
      cores: 8
      hostname: GDaQNF
      name: Java HotSpot(TM) 64-Bit Server VM
      osArch: amd64
      osName: Windows 10
      vendor: Oracle Corporation
      version: 25.131-b11
      Parameters(numPoints -> 10000): 86.021998
      Parameters(numPoints -> 20000): 346.168134
      Parameters(numPoints -> 30000): 782.997272
      Parameters(numPoints -> 40000): 1392.659454

      ::Benchmark Points.divAndConq::
      cores: 8
      hostname: GDaQNF
      name: Java HotSpot(TM) 64-Bit Server VM
      osArch: amd64
      osName: Windows 10
      vendor: Oracle Corporation
      version: 25.131-b11
      Parameters(numPoints -> 10000): 27.676452
      Parameters(numPoints -> 20000): 89.352559
      Parameters(numPoints -> 30000): 187.294773
      Parameters(numPoints -> 40000): 318.6669
     */
  } // eo-performance

  performance of "Points3D" in {
    measure method "divAndConq" in {
      using(e_sizes) in { r => daq(r)}
      using(pow2sizes) in { r => daq(r)}
    }
    measure method "bruteForce" in {
      using(e_sizes) in { r => brf(r)}
      using(pow2sizes) in { r => brf(r)}
    }


    def daq(r: Int): Unit = {
      //TODO: Factor out point creation
      val P = Array.fill(r) {
        Point3D(
          Random.nextDouble * 100,
          Random.nextDouble * 100,
          Random.nextDouble * 100)
      }
      Points3D(P).closestPair()
    }

    def brf(r: Int): Unit = {
      //TODO: Factor out point creation
      val P = Array.fill(r) {
        Point3D(
          Random.nextDouble * 100,
          Random.nextDouble * 100,
          Random.nextDouble * 100)
      }
      NaiveBrute.closestPair3D(P)
    }
  }
}
