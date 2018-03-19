package languages.automata

import languages.{Language, Regular, Word}

trait FAState {
  def on(c: Char): Option[FAState]
}

trait FiniteAutomaton extends Language[Regular] {
  val q0: FAState
  val fqs: Set[FAState]

  // Provided
  override def contains(w: Word): Boolean = accepts(w)
  // Required

  def delta(c: Char, q: FAState): Option[FAState]
  def deltaHat(w: Word, q: FAState): Option[FAState]
  def accepts(w: Word): Boolean
}

object FiniteAutomaton {
  trait Property
  trait Set extends Property
  trait Unset extends Property

  // States, Initial State, Final States, Alphabet
  /*class FABuilder[S, IS, FS, A] {
    def withStates[S <: Unset, IS, FS, A, State](s: Seq[State]): FABuilder = {

    }
  }*/

  //def builder(): FABuilder[Unset, Unset, Unset, Unset] = new FABuilder()

  // IsoMorphic: A and B are ISOMORPHIC if they have similar structure.
  //    So how do we define similar?
  //    (identical structure is definitely similar)

  //def withStates[S](s: )
}