package languages.automata

import languages.Word

class NonDeterministicFA(override val q0: FAState,
                         override val fQs: Set[FAState]) extends FiniteAutomaton {

  override def delta(c: Char, q: FAState): Option[FAState] = q.on(c)
  override def deltaHat(w: Word, q: FAState): Option[FAState] = if(w.isEmpty) Some(q)
  else delta(w.head, q) match {
    case Some(x) => deltaHat(w.tail, x)
    case None    => None
  }

  override def accepts(w: Word): Boolean = deltaHat(w, q0) match {
    case Some(q) => fQs.contains(q)
    case None    => false
  }
}
