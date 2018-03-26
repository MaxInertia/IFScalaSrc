package experimental.nearest

import org.scalatest._
import org.scalatest.FunSpec

import scala.util.Random

/**
  * Created by Dorian Thiessen on 2018-03-25.
  */
class ResultVerifier extends FunSpec {
  def repeatComparison2D(points: Int, repeats: Int): Unit = {
    var failures = 0
    for (i <- 0 until repeats) {
      val P = Array.fill(points) {
        Point2D(
          Random.nextDouble * 100,
          Random.nextDouble * 100)
      }

      // Efficient Algorithm Result
      val effResult = Points2D(P).closestPair()

      // Brute Force Result
      val bfResult = NaiveBrute.closestPair2D(P)

      if (effResult._3 != bfResult._3) {
        if (bfResult._3 < effResult._3) print("Failed: ")
        else print("FAILED(eff>bf) ???? : ")
        println(s"${effResult._3} != ${bfResult._3}")
        failures += 1
      }
    }

    assert(failures == 0, s"Ran $repeats instances, failed $failures times.")
  }

  def repeatComparison3D(points: Int, repeats: Int): Unit = {
    var failures = 0
    for (i <- 0 until repeats) {
      val P = Array.fill(points) {
        Point3D(
          Random.nextDouble * 100,
          Random.nextDouble * 100,
          Random.nextDouble * 100)
      }

      // Efficient Algorithm Result
      val effResult = Points3D(P).closestPair()

      // Brute Force Result
      val bfResult = NaiveBrute.closestPair3D(P)

      if (effResult._3 != bfResult._3) {
        if (bfResult._3 < effResult._3) print("Failed: ")
        else print("FAILED(eff>bf) ???? : ")
        println(s"${effResult._3} != ${bfResult._3}")
        failures += 1
      }
    }

    assert(failures == 0, s"Ran $repeats instances, failed $failures times.")
  }

  describe("Nearest Pair Algorithm") {
    describe("matches brute force comparison results") {
      describe("3D") {
        it("with 10 points") {
          repeatComparison3D(10, 10000)
        }
        it("with 100 points") {
          repeatComparison3D(100, 10000)
        }
        it("with 1000 points") {
          repeatComparison3D(1000, 1000)
        }
      }
      describe("2D") {
        it("with 10 points") {
          repeatComparison2D(10, 10000)
        }
        it("with 100 points") {
          repeatComparison2D(100, 10000)
        }
        it("with 1000 points") {
          repeatComparison2D(1000, 1000)
        }
      }
    }
  }

  describe("K-Nearest Algorithm") {
    describe("2D") {
      it("with 10 points") {
        val P = Array.fill(10) {Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
        val ps = Points2D(P)
        ps.kClosest(3)
      }
    }
  }
}
