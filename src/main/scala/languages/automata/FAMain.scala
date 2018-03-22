package languages.automata

object FAMain extends App {
  //dfa()
  /*nfa()

  def dfa(): Unit = {
    class State(transitions: Map[Char, State] = Map()) extends FAState {
      override def on(c: Char): Option[FAState] = transitions.get(c)
      def +>(c: Char, s: State) = new State(transitions + (c -> s))

      override def toString: String = "{"+ transitions.toString +"}"
    }

    val q2 = new State()
    val q1 = new State() +> ('b', q2)
    val q0 = new State() +> ('a', q1)

    val dfa = new DeterministicFA(q0, Set(q2))

    println(dfa.deltaHat("", q0).toString)
    println(dfa.deltaHat("a", q0).toString)
    println(dfa.deltaHat("ab", q0).toString)

    def acceptanceCheck(in: String): Boolean = {
      val result = dfa.accepts(in)
      println(s"dfa accepts '$in': $result")
      result
    }
    assert(acceptanceCheck("ab"))
    assert(acceptanceCheck("b"))
    assert(!acceptanceCheck("a"))
    assert(!acceptanceCheck("abc"))
    assert(!acceptanceCheck("ba"))
    assert(!acceptanceCheck(""))
  }

  def nfa(): Unit = {
    class State(ids: Seq[String], transitions: Map[Char, State] = Map()) extends FAState {
      override def on(c: Char): Option[FAState] = transitions.get(c)
      def +>(c: Char, s: State) = new State(id, transitions + (c -> s))
      override def toString: String = "{"+ transitions.toString +"}"
    }

    val q2 = new State("q2")
    val q1 = new State("q1") +> ('b', q2)
    val q0 = new State("q0") +> ('a', q1) +> ('b', q2)

    val nfa = new NonDeterministicFA(q0, Set(q2))

    println(nfa.deltaHat("", q0).toString)
    println(nfa.deltaHat("a", q0).toString)
    println(nfa.deltaHat("ab", q0).toString)

    def acceptanceCheck(in: String): Boolean = {
      val result = nfa.accepts(in)
      println(s"nfa accepts '$in': $result")
      result
    }
    assert(acceptanceCheck("ab"))
    assert(acceptanceCheck("b"))
    assert(!acceptanceCheck("a"))
    assert(!acceptanceCheck("abc"))
    assert(!acceptanceCheck("ba"))
    assert(!acceptanceCheck(""))
  }*/
}
