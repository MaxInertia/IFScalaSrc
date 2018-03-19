package languages.automata
import languages.Word

class DeterministicFA(override val q0: FAState,
                      override val fqs: Set[FAState]) extends FiniteAutomaton {

  def delta(c: Char, q: FAState): Option[FAState] = q.on(c)
  def deltaHat(w: Word, q: FAState): Option[FAState] =
    if(w.isEmpty) Some(q)
    else delta(w.head, q) match {
      case Some(x) => deltaHat(w.tail, x)
      case None    => None
    }

  def accepts(w: Word): Boolean = ???
}
