package languages.automata

object FAMain extends App {
  class State(transitions: Map[Char, State] = Map()) extends FAState {
    override def on(c: Char): Option[FAState] = transitions.get(c)
    def +>(c: Char, s: State) = new State(transitions + (c -> s))

    override def toString: String = "{"+ transitions.toString +"}"
  }


  val q2 = new State()
  val q1 = new State() +> ('b', q2)
  val q0 = new State() +> ('a', q1)

  val dfa = new DeterministicFA(q0, Set(q2))

  //println(dfa.delta('a', q0).toString)

  println(dfa.deltaHat("", q0).toString)
  println(dfa.deltaHat("a", q0).toString)
  println(dfa.deltaHat("ab", q0).toString)
}
