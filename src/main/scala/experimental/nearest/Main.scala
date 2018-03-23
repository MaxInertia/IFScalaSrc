package experimental.nearest

import scala.util.Random

// SAT Solvers

object Main extends App {

  val n = 200
  val points = Array.fill(n){Point2D(Random.nextDouble * 100*n, Random.nextDouble * 100*n)}

  // Efficient Algorithm Result
  val effResult = Points(points).closestPair()

  // Brute Force Result
  val bfResult = NaiveBrute.closestPair(points)

  Util.showAndCompare(effResult, bfResult)

  // ComparisonRepeater
  Util.repeatComparison(1000, 100)
}

object Util {
  type ResultTriple = (Point2D, Point2D, Double)

  def show(closestPair: ResultTriple): Unit =
    println(closestPair._1.toString +" <-> "+
      closestPair._2.toString +
      " : "+ closestPair._3)

  def compare(a: ResultTriple, b: ResultTriple): Unit = {
    if(a._3 != b._3) println("DIFFERENT RESULTS!")
  }

  def showAndCompare(eff: ResultTriple, bf: ResultTriple): Unit = {
    bar()
    print("Eff-Algorithm Result: ")
    show(eff)
    print("BrF-Algorithm Result: ")
    show(bf)
    compare(eff, bf)
    bar()
  }

  def bar(): Unit = {
    for(i<- 0 to 128) print('-')
    println
  }

  def repeatComparison(repititions: Int, points: Int): Unit = {
    var failures = 0
    for(i <- 0 until points) {
      val P = Array.fill(points){Point2D(
        Random.nextDouble * 100*points,
        Random.nextDouble * 100*points)}

      // Efficient Algorithm Result
      val effResult = Points(P).closestPair()

      // Brute Force Result
      val bfResult = NaiveBrute.closestPair(P)

      if(effResult._3 != bfResult._3) {
        println(s"Failed: ${effResult._3} != ${bfResult._3}")
        failures += 1
      }
    }
    println(s"Ran $repititions instances, failed $failures times.")
  }
}