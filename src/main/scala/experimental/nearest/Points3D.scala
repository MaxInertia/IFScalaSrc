package experimental.nearest

/**
  * Created by Dorian Thiessen on 2018-03-24.
  */
case class Points3D(P: Array[Point3D])  {
  def closestPair(): (Point3D, Point3D, Double) = {

    def loop(Px: Array[Point3D], Py: Array[Point3D], Pz: Array[Point3D]): (Point3D, Point3D) = {
      //op(Px, Py)

      //println(s"|Px|=${Px.length}\n|Py|=${Py.length}\n|Pz|=${Pz.length}\n")

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

      val Qx = Array.fill[Point3D](half){null}
      var Qy = Array[Point3D]()
      var Qz = Array[Point3D]()

      val Rx = Array.fill[Point3D](Px.length - half){null}
      var Ry = Array[Point3D]()
      var Rz = Array[Point3D]()

      // X's: Qx and Rx
      for(i <- 0 until half) {
        Qx(i) = Px(i)
      }
      for(i <- half until Px.length) {
        Rx(i - half) = Px(i)
      }

      // Qy and Ry
      //assert(Py.length == Pz.length)
      for(i <- Py.indices) {
        val py = Py(i)
        if (Py(i).xsIndex < half) {
          py.ysIndex = Qy.length + 1
          Qy = Qy :+ py
        } else {
          py.ysIndex = Ry.length + 1
          Ry = Ry :+ py
        }
      }

      // Qz and Rz
      for(i <- Pz.indices) {
        val pz = Pz(i)
        if(Pz(i).xsIndex < half) {
          pz.zsIndex = Qz.length + 1
          Qz = Qz :+ pz
        } else {
          pz.xsIndex = pz.xsIndex - half
          pz.zsIndex = Rz.length + 1
          Rz = Rz :+ pz
        }
      }

      // Recursive calls
      val (q1, q2) = loop(Qx, Qy, Qz)
      val (r1, r2) = loop(Rx, Ry, Rz)

      // Find and store min from recursive calls
      def min(a: Double, b: Double): Double = if(a<b) a else b
      var (p1, p2): (Point3D, Point3D) = (null, null)
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
        for(j <- (i+1) until (i+16)) { // Max 16^2 iterations
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

    // CONSTRUCT Px, Py, and Pz
    var i = 0
    val Px = P.sortWith(_.x < _.x) // Sort points: O(n log n)
    Px.foreach{p => p.xsIndex = i; i+=1} // Init each points x-index
    i = 0
    val Py = Px.sortWith(_.y < _.y) // Sort points: O(n log n)
    Py.foreach{p => p.ysIndex = i; i+=1} // Init each points y-index
    i = 0
    val Pz = P.sortWith(_.z < _.z) // Sort points: O(n log n)
    Pz.foreach{p => p.zsIndex = i; i+=1} // Init each points z-index

    //println(s"|Px|=${Px.length}\n|Py|=${Py.length}\n|Pz|=${Pz.length}\n")

    // CALL LOOP
    val (p1, p2) = loop(Px, Py, Pz)

    // RETURN
    (p1, p2, p1.distTo(p2))
  }
}
