package experimental.nearest

import scala.concurrent.Await
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
    val npc = Points2D(rawPoints)
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
  /*nearestForAll(100)
  nearestForAll(1000)
  nearestForAll(2000)
  nearestForAll(3000)
  nearestForAll(4000)
  nearestForAll(10000)*/

  def parallelNearestPair(n: Int): Unit = {

    val rawPoints = Array.fill(n){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}
    val npc = ParPoints(rawPoints)
    val futurePX = npc.xSort(npc.P)
    val futurePY = npc.ySort(npc.P)

    val t0 = System.nanoTime()
    import scala.concurrent.duration._
    val pX = Await.result(futurePX, 2.seconds)
    val pY = Await.result(futurePY, 2.seconds)

    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    for(p <- pX) print(p+", ")
    println
    for(p <- pY) print(p+", ")
    println

    val t2 = System.nanoTime()
    val pX2 = rawPoints.sortWith(_.x < _.x)
    val pY2 = rawPoints.sortWith(_.y < _.y)
    val t3 = System.nanoTime()
    println("Elapsed time: " + (t3 - t2) + "ns")
  }
  //parallelNearestPair(10)
  /*for(i <- Array(1000, 2000, 5000, 10000, 20000, 50000, 100000)) {
    timeCompare(i)
    println
  }*/

  def timeCompare(n: Int): Unit = {
    import scala.concurrent.duration._

    println(s"Points: $n")
    val rawPoints = Array.fill(n){Point2D(Random.nextDouble * 100, Random.nextDouble * 100)}

    val npc = ParPoints(rawPoints)
    val futurePX = npc.xSort(npc.P)
    val futurePY = npc.ySort(npc.P)

    val t0 = System.nanoTime()
    val pX = Await.result(futurePX, 2.seconds)
    val pY = Await.result(futurePY, 2.seconds)
    val t1 = System.nanoTime()

    val t2 = System.nanoTime()
    val pX2 = rawPoints.sortWith(_.x < _.x)
    val pY2 = rawPoints.sortWith(_.y < _.y)
    val t3 = System.nanoTime()

    println(s"Par: ${t1-t0}ns")
    println(s"Seq: ${t3-t2}ns")
    if((t1-t0) < (t3 - t2)) println("PAR WINS")
    else println("SEQ WINS")
  }

  def threeD(n: Int): Unit = {
    val rawPoints = Array.fill(n){Point3D(
      Random.nextDouble * 100,
      Random.nextDouble * 100,
      Random.nextDouble * 100)}

    val npc = Points3D(rawPoints)

    val t0 = System.nanoTime()
    val (p1, p2, dist) = npc.closestPair()
    val t1 = System.nanoTime()

    val t2 = System.nanoTime()
    val (bf1, bf2, bfDist) = NaiveBrute.closestPair3D(rawPoints)
    val t3 = System.nanoTime()

    println(s"Eff-3D: ${t1-t0}ns")
    println(s"\t$p1 - $p2 @ $dist")
    println(s"BrF-3D: ${t3-t2}ns")
    println(s"\t$bf1 - $bf2 @ $bfDist")
    assert(dist == bfDist)
  }

  //threeD(100)

  //Util.repeatComparison3D(10, 1000)
  Util.repeatComparison3D(1000, 1000)
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
      val effResult = Points2D(P).closestPair()

      // Brute Force Result
      val bfResult = NaiveBrute.closestPair2D(P)

      if(effResult._3 != bfResult._3) {
        if(bfResult._3 < effResult._3) print("Failed: ")
        else print("FAILED(eff>bf) ???? : ")
        println(s"${effResult._3} != ${bfResult._3}")
        failures += 1
      }
    }
    println(s"Ran $repititions instances, failed $failures times.")
  }

  def repeatComparison3D(points: Int, repititions: Int): Unit = {
    var failures = 0
    for(i <- 0 until repititions) {
      val P = Array.fill(points){Point3D(
        Random.nextDouble * 100,
        Random.nextDouble * 100,
        Random.nextDouble * 100)}

      // Efficient Algorithm Result
      val effResult = Points3D(P).closestPair()

      // Brute Force Result
      val bfResult = NaiveBrute.closestPair3D(P)

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
      val P = Points2D(points)
      val effResult = P.closestPair()

      // Brute Force Result
      val bfResult = NaiveBrute.closestPair2D(points)

      if(check(effResult, bfResult)) {
        P.op = fn
        val effResult = P.closestPair()
        showAndCompare(effResult, bfResult)
        return
      }
    }
  }
}