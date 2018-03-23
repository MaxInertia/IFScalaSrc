package experimental

import scala.util.Random

/**
  * Created by Dorian Thiessen on 2018-03-22.
  */
object NearestKPoints extends App {
  val UNSET = -1
  case class Point2D(x: Int, y: Int) {
    var xsIndex: Int = UNSET
    var ysIndex: Int = UNSET
  }


  val n = 200
  val points = Array.fill(n){Point2D(Random.nextInt(n*100), Random.nextInt(n*100))}
  println(points.length)

  def d(p1: Point2D, p2: Point2D): Double = math.sqrt(
      math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2)
  )

  // Nearest points implementation
  // Returns: (p1, p2, distance)
  def nearestPoints(P: Array[Point2D]): (Point2D, Point2D, Double) = {

    def loop(Px: Array[Point2D], Py: Array[Point2D]): (Point2D, Point2D) = {
      def details(): Unit = {
        println("Px: " + Px.length)
        print("\t")
        Px.foreach(p => print(s"(${p.x}, ${p.y}), "))
        print("\n")

        println("Py: " + Py.length)
        print("\t")
        Py.foreach(p => print(s"(${p.x}, ${p.y}), "))
        print("\n\n\n")
      }
      //details()

      if(Px.length == 3) {
        //println("Px length: "+Px.length)
        val d01 = d(Px(0), Px(1))
        val d02 = d(Px(0), Px(2))
        val d12 = d(Px(1), Px(2))
        if(d01 < d02) {
          if(d01 < d12) return (Px(0), Px(1))
          else return (Px(1), Px(2))
        } else {
          if(d02 < d12) return (Px(0), Px(2))
          else return (Px(1), Px(2))
        }
     } else if(Px.length == 2) return (Px(0), Px(1))
      //println("Px length: "+Px.length)

      // TODO: Improve split?
      // Shouldn't have to traverse the list more than once.
      // TODO: Don't need this step at all? Combined with next step
      //val (q, r) = Px.splitAt(Px.length/2)

      val half = Px.length/2

      var Qx = Array.fill[Point2D](half){null}
      var Qy = Array[Point2D]()
      var Rx = Array.fill[Point2D](Px.length - half){null}
      var Ry = Array[Point2D]()

      // X's
      for(i <- 0 until half) {
        //Px(i).xsIndex = i
        Qx(i) = Px(i)
      }
      for(i <- half until Px.length) {
        //Px(i).xsIndex = i - half
        Rx(i - half) = Px(i)
      }

      // Y's
      var (qc, rc) = (0, 0) // q-counter and r-counter
      for(i <- Py.indices) {
        val p = Py(i)
        if(Py(i).xsIndex < half) {
          //p.ysIndex = Qy.length + 1
          Qy = Qy :+ p
        } else {
          //p.ysIndex = Ry.length + 1
          Ry = Ry :+ p
        }
      }

      def show(): Unit = {
        println("Qx: " + Qx.length)
        print("\t")
        Qx.foreach(p => print(s"(${p.x}, ${p.y}), "))
        print("\n")

        println("Qy: " + Qy.length)
        print("\t")
        Qy.foreach(p => print(s"(${p.x}, ${p.y}), "))
        print("\n")

        println("Rx: " + Rx.length)
        print("\t")
        Rx.foreach(p => print(s"(${p.x}, ${p.y}), "))
        print("\n")

        println("Ry: " + Ry.length)
        print("\t")
        Ry.foreach(p => print(s"(${p.x}, ${p.y}), "))
        print("\n")
      }
      //show()

      val (q1, q2) = loop(Qx, Qy)
      val (r1, r2) = loop(Rx, Ry)
      //println(q1 +" "+ q2 +" "+ r1 +" "+ r2)
      def min(a: Double, b: Double): Double = if(a<b) a else b

      var (p1, p2): (Point2D, Point2D) = (null, null)
      var delta = Double.MaxValue // Minimum distance found in this stage
      if(d(q1, q2) < d(r1, r2)) {
        delta = d(q1, q2)
        p1 = q1
        p2 = q2
      } else {
        delta = d(r1, r2)
        p1 = r1
        p2 = r2
      }

      val xEndQ = Qx.last.x
      // TODO: Optimize by Q.reverse.takewhile and R.takewhile
      val S = Py.filter(p => {math.abs(p.x - xEndQ) <= delta})

      println("S: "+ S.length)
      print("\t")
      S.foreach(p => print(s"(${p.x}, ${p.y}), "))
      print("\n\n")


      for(i <- S.indices) {
        for(j <- (i+1) until i+16) {
          if(i != j && j < S.length) {
            val dist = d(S(i), S(j))
            if (dist < delta) {
              delta = dist
              p1 = S(i)
              p2 = S(j)
            }
          }
        }
      }

      (p1, p2)
    }

    // CONSTRUCT Px and Py
    val Px = P.sortWith(_.x < _.x)
    var i = 0
    Px.foreach{p => p.xsIndex = i; i+=1}
    val Py = Px.sortWith(_.y < _.y)
    i = 0
    Py.foreach{p => p.ysIndex = i; i+=1}
    // CALL LOOP
    val (p1, p2) = loop(Px, Py)
    // RETURN
    (p1, p2, d(p1, p2))
  }

  val (p1, p2, dist) = nearestPoints(points)
  println(s"Closest Pair: $p1 and $p2, distance: $dist")

  val (bruteP1, bruteP2, bruteDist) = bruteClosest(points)
  println(s"BruteF Pair: $bruteP1 and $bruteP2, distance: $bruteDist")

  println("\nPoints:")
  points.foreach(p => print(s"(${p.x}, ${p.y}), "))

  // TODO: Determine if this can be generalized to N-dimensional-points
  // 1D: O(nlogn) sort + single traversal
  // 2D: O(nlogn) sort + DAQ with O(n) work per level (including combining results)
  // 3D: Is it possible to make the work per level O(n)?
  // 4D+ ???

  def bruteClosest(P: Array[Point2D]): (Point2D, Point2D, Double) = {
    // TODO: Implement Brute-Force Closest-Pair solver, use to verify clever solution
    var (bp1, bp2): (Point2D, Point2D) = (null, null)
    var smallestDistance = Double.MaxValue
    for{
      i <- P.indices
      j <- P.indices
      if i != j
    } {
      val dist = d(P(i), P(j))
      if(dist < smallestDistance) {
        smallestDistance = dist
        bp1 = P(i)
        bp2 = P(j)
      }
    }
    (bp1, bp2, smallestDistance)
  }
}
