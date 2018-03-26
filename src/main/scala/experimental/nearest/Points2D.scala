package experimental.nearest


case class Points2D(P: Array[Point2D]) {
  // Purpose of this class was to wrap the Points in the extended point class
  // time will tell if there is a better way to do it.

  var op: (Array[Point2D], Array[Point2D]) => Unit = (_, _) => {}

  def closestPair(): (Point2D, Point2D, Double) = {

    def loop(Px: Array[Point2D], Py: Array[Point2D]): (Point2D, Point2D) = {
      op(Px, Py)

      if(Px.length == 3) {
        val d01 = Px(0).distTo(Px(1))
        val d02 = Px(0).distTo(Px(2))
        val d12 = Px(1).distTo(Px(2))
        if(d01 < d02) {
          if(d01 < d12) return (Px(0), Px(1))
          else return (Px(1), Px(2))
        } else {
          if(d02 < d12) return (Px(0), Px(2))
          else return (Px(1), Px(2))
        }
      } else if(Px.length == 2) return (Px(0), Px(1))

      val half = Px.length/2

      var Qx = Array.fill[Point2D](half){null}
      var Qy = Array[Point2D]()
      var Rx = Array.fill[Point2D](Px.length - half){null}
      var Ry = Array[Point2D]()

      // X's: Qx and Rx
      for(i <- 0 until half) {
        Qx(i) = Px(i)
      }
      for(i <- half until Px.length) {
        Rx(i - half) = Px(i)
      }

      // Y's: Qy and Ry
      for(i <- Py.indices) {
        val p = Py(i)
        if(Py(i).xsIndex < half) {
          p.ysIndex = Qy.length + 1
          Qy = Qy :+ p
        } else {
          p.ysIndex = Ry.length + 1
          Ry = Ry :+ p
        }
        if(p.xsIndex >= half) p.xsIndex = p.xsIndex - half
      }

      // Recursive calls
      val (q1, q2) = loop(Qx, Qy)
      val (r1, r2) = loop(Rx, Ry)

      // Find and store min from recursive calls
      def min(a: Double, b: Double): Double = if(a<b) a else b
      var (p1, p2): (Point2D, Point2D) = (null, null)
      var delta = Double.MaxValue
      if(q1.distTo(q2) < r1.distTo(r2)) {
        delta = q1.distTo(q2)
        p1 = q1
        p2 = q2
      } else {
        delta = r1.distTo(r2)
        p1 = r1
        p2 = r2
      }

      // Compare min from recursive calls with possible overlap
      val xEndQ = Qx.last.x
      // TODO: Optimize by Q.reverse.takewhile and R.takewhile
      val S = Py.filter(p => {math.abs(p.x - xEndQ) <= delta})
      // TODO: Optimize by not comparing pairs of points from the same recursive call (Q's and R's)
      for(i <- S.indices) {
        for(j <- (i+1) until i+17) { // Max 16 iterations
          if(i != j && j < S.length) {
            val dist = S(i).distTo(S(j))
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
    var i = 0
    val Px = P.sortWith(_.x < _.x) // Sort points: O(n log n)
    Px.foreach{p => p.xsIndex = i; i+=1} // Init each points x-index
    i = 0
    val Py = Px.sortWith(_.y < _.y) // Sort points: O(n log n)
    Py.foreach{p => p.ysIndex = i; i+=1} // Init each points y-index

    // CALL LOOP
    val (p1, p2) = loop(Px, Py)

    // RETURN
    (p1, p2, p1.distTo(p2))
  }

  def kClosest(k: Int): Unit = {

    def loop(Px: Array[Point2D], Py: Array[Point2D]): (Point2D, Point2D) = {
      op(Px, Py)

      if(Px.length == 3) {
        val d01 = Px(0).distTo(Px(1))
        val d02 = Px(0).distTo(Px(2))
        val d12 = Px(1).distTo(Px(2))
        if(d01 < d02) {
          if(d01 < d12) return (Px(0), Px(1))
          else return (Px(1), Px(2))
        } else {
          if(d02 < d12) return (Px(0), Px(2))
          else return (Px(1), Px(2))
        }
      } else if(Px.length == 2) return (Px(0), Px(1))

      val half = Px.length/2

      var Qx = Array.fill[Point2D](half){null}
      var Qy = Array[Point2D]()
      var Rx = Array.fill[Point2D](Px.length - half){null}
      var Ry = Array[Point2D]()

      // X's: Qx and Rx
      for(i <- 0 until half) {
        Qx(i) = Px(i)
      }
      for(i <- half until Px.length) {
        Rx(i - half) = Px(i)
      }

      // Y's: Qy and Ry
      for(i <- Py.indices) {
        val p = Py(i)
        if(Py(i).xsIndex < half) {
          p.ysIndex = Qy.length + 1
          Qy = Qy :+ p
        } else {
          p.ysIndex = Ry.length + 1
          Ry = Ry :+ p
        }
        if(p.xsIndex >= half) p.xsIndex = p.xsIndex - half
      }

      // Recursive calls
      val (q1, q2) = loop(Qx, Qy)
      val (r1, r2) = loop(Rx, Ry)

      // Find and store min from recursive calls
      def min(a: Double, b: Double): Double = if(a<b) a else b
      var (p1, p2): (Point2D, Point2D) = (null, null)
      var delta = Double.MaxValue
      if(q1.distTo(q2) < r1.distTo(r2)) {
        delta = q1.distTo(q2)
        p1 = q1
        p2 = q2
      } else {
        delta = r1.distTo(r2)
        p1 = r1
        p2 = r2
      }

      // Compare min from recursive calls with possible overlap
      val xEndQ = Qx.last.x
      // TODO: Optimize by Q.reverse.takewhile and R.takewhile
      val S = Py.filter(p => {math.abs(p.x - xEndQ) <= delta})
      // TODO: Optimize by not comparing pairs of points from the same recursive call (Q's and R's)
      for(i <- S.indices) {
        for(j <- (i+1) until i+17) { // Max 16 iterations
          if(i != j && j < S.length) {
            val dist = S(i).distTo(S(j))
            if (dist < delta) {
              delta = dist
              p1 = S(i)
              p2 = S(j)
            }
          }
        }
      }

      p1.setNearest(p2)
      (p1, p2)
    }

    // CONSTRUCT Px and Py
    var i = 0
    val Px = P.sortWith(_.x < _.x) // Sort points: O(n log n)
    Px.foreach{p => p.xsIndex = i; i+=1} // Init each points x-index
    i = 0
    val Py = Px.sortWith(_.y < _.y) // Sort points: O(n log n)
    Py.foreach{p => p.ysIndex = i; i+=1} // Init each points y-index

    // CALL LOOP
    val (p1, p2) = loop(Px, Py)

    // RETURN
    (p1, p2, p1.distTo(p2))
  }

}

