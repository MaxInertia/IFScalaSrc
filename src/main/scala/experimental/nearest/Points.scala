package experimental.nearest

case class Points(P: Array[Point2D]) {
  def closestPair(): (Point2D, Point2D, Double) = {

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
      if(q1.distTo(q2) < r1.distTo(r2)) {
        delta = q1.distTo(q2)
        p1 = q1
        p2 = q2
      } else {
        delta = r1.distTo(r2)
        p1 = r1
        p2 = r2
      }

      val xEndQ = Qx.last.x
      // TODO: Optimize by Q.reverse.takewhile and R.takewhile
      val S = Py.filter(p => {math.abs(p.x - xEndQ) <= delta})

      /*println("S: "+ S.length)
      print("\t")
      S.foreach(p => print(s"(${p.x}, ${p.y}), "))
      print("\n\n")*/

      for(i <- S.indices) {
        for(j <- (i+1) until i+16) {
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
    val Px = P.sortWith(_.x < _.x)
    var i = 0
    Px.foreach{p => p.xsIndex = i; i+=1}
    val Py = Px.sortWith(_.y < _.y)
    i = 0
    Py.foreach{p => p.ysIndex = i; i+=1}
    // CALL LOOP
    val (p1, p2) = loop(Px, Py)
    // RETURN
    (p1, p2, p1.distTo(p2))
  }

}

object Points {}
