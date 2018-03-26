package experimental.fp

/**
  * Created by Dorian Thiessen on 2018-03-22.
  */
case class State[S, A](f: S => (A, S)) {
  def apply(s: S) = f(s)

  def map[B](g: A => B): State[S, B] = State[S, B] {
    s => {
      val (a, s2) = f(s)
      (g(a), s2)
    }
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = State[S, B](
    s => {
      val (a, t) = f(s)
      g(a)(t)
    }
  )

  /*def focus[T](l: Lens[T,S]) = State[T, A](
    t => {
      val (a, s) = f(l.get(t))
      //(a, l.set(s, t))
    }
  )*/

  // ...
}

object State {
  def pure[S, A](a: A): State[S, A] =
    State[S, A](s => (a,s))

  def put[S](s: S): State[S, Unit] =
    State[S, Unit](_ => ((), s))

  def init[S]: State[S, S] =
    State[S, S](s => (s, s))
}

