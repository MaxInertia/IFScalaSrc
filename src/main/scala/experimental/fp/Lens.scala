package experimental.fp

/**
  * Created by Dorian Thiessen on 2018-03-22.
  */
object Lens {
  implicit def lensAsState[S, A](l: Lens[S, A]) =
    State[S, A] {
      s => (l.get(s), s)
    }
}

// ---

case class Lens[S, A]() (
  g: S => A,
  s: (A, S) => S
) {
  // Get component of state
  def get(s: S): A = g(s)

  // Set component of state; returns new instance of S with modified A
  /*def set(a: A, s: S): S = s(a, s)

  def := (a: A) =
    State[S, A] (
      s => (a, set(a, s))
    )

  def %= (f: A => A): State[S, A] = for {
    x <- this
    y <- this := f(x)
  } yield y*/
}

case class NumLens[S, C : Numeric](l: Lens[S, C]) {
  val N = implicitly[Numeric[C]]

}