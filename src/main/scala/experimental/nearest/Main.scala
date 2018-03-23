package experimental.nearest

import scala.util.Random

// SAT Solvers

object Main extends App {
  def nearestPair(): Unit = {
    /*
    val n = 40
    val points = Array.fill(n){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}

    // Efficient Algorithm Result
    //val effResult = Points(points).closestPair()

    // Brute Force Result
    val bfResult = NaiveBrute.closestPair1(points)

    //Util.showAndCompare(effResult, bfResult)

    val bf2Result = NaiveBrute.closestPair(points)

    Util.showAndCompare(bf2Result, bfResult)
    */
    // ComparisonRepeater
    //Util.repeatComparison(20, 1000)

    def view(a: Array[Point2D], b: Array[Point2D]): Unit = {
      println("Px: " + a.length)
      print("\t")
      a.foreach(p => print(s"(${p.x}, ${p.y}), "))
      print("\n")

      println("Py: " + b.length)
      print("\t")
      b.foreach(p => print(s"(${p.x}, ${p.y}), "))
      print("\n\n")
    }

    /*Util.atCaseDo(13, 10000)(
      fn = view,
      (a, b) => a._3 != b._3
    )*/

    Util.repeatComparison(1000, 1000)
  }
  //nearestPair()

  def nearestForAll(n: Int): Unit = {
    val rawPoints = Array.fill(n){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
    val npc = Points(rawPoints)
    npc.closestToEach()
    var missing = 0
    for(p <- npc.P) {
      if(p.getNearest._1 == null) {
        //println("Nearest was not set for this point!")
        missing += 1
      }
    }
    println(s"$missing out of $n points are missing nearest neighbors")
  }
  nearestForAll(100)
  nearestForAll(1000)
  nearestForAll(2000)
  nearestForAll(3000)
  nearestForAll(4000)
  nearestForAll(10000)
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

  def repeatComparison(points: Int, repititions: Int): Unit = {
    var failures = 0
    for(i <- 0 until repititions) {
      val P = Array.fill(points){Point2D(
        Random.nextDouble * 100*points,
        Random.nextDouble * 100*points)}

      // Efficient Algorithm Result
      val effResult = Points(P).closestPair()

      // Brute Force Result
      val bfResult = NaiveBrute.closestPair2(P)

      if(effResult._3 != bfResult._3) {
        if(bfResult._3 < effResult._3) print("Failed: ")
        else print("FAILED(eff>bf) ???? : ")
        println(s"${effResult._3} != ${bfResult._3}")
        failures += 1
      }
    }
    println(s"Ran $repititions instances, failed $failures times.")
  }

  def atCaseDo[T](pCount: Int, maxAttempts: Int)
                 (fn: (Array[Point2D], Array[Point2D]) => Unit,
                  check: (ResultTriple, ResultTriple) => Boolean): Unit = {
    for (i <- 0 until maxAttempts) {
      // Create random points
      val points = Array.fill(pCount) {
        Point2D(
          Random.nextDouble * 100,
          Random.nextDouble * 100)
      }

      // Efficient Algorithm Result
      val P = Points(points)
      val effResult = P.closestPair()

      // Brute Force Result
      val bfResult = NaiveBrute.closestPair2(points)

      if(check(effResult, bfResult)) {
        P.op = fn
        val effResult = P.closestPair()
        showAndCompare(effResult, bfResult)
        return
      }
    }
  }
}