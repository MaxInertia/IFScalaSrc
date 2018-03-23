package experimental

/**
  * Created by Dorian Thiessen on 2018-03-22.
  */
object Main extends App {
  //type State = Int
  val s0 = State[Int, Int]((s) => (s, 1))
  println(s0)

  println(s"s0(2) = ${s0(2)}")
  println(s"s0.map(_%2==0)(2) = ${s0.map(_%2==0)(1)}")

  def mod[S](f: S => S): State[S, Unit] = for {
    s <- State.init
    _ <- State.put(f(s))
  } yield ()

  def fresh: State[Int, Int] = for {
    x <- State.init
    _ <- State.put(x + 1)
  } yield x

  println(fresh(17))
  println(State.put[Int](2)(3))

  println("-----")

  //val l = Lens[Int, Int]()
  //println(l)
}

