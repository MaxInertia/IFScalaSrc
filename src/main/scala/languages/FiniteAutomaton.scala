package languages

trait FiniteAutomaton extends Language[Regular] {
  // Provided
  override def contains(w: Word): Boolean = accepts(w)
  // Required
  type State
  def delta(c: Char, q: State): State
  def deltaHat(w: Word, q: State): State
  def accepts(w: Word): Boolean
}

// TODO: Implement DFA, NFA, and epsilon-NFA (Remove abstract modifier)
abstract class DeterministicFA extends FiniteAutomaton
abstract class NonDeterministicFA extends FiniteAutomaton
abstract class EpsilonNFA extends NonDeterministicFA

object FiniteAutomaton {
  trait Property
  trait Set extends Property
  trait Unset extends Property

  // States, Initial State, Final States, Alphabet
  class FABuilder[S, IS, FS, A] {

  }

  def builder(): FABuilder[Unset, Unset, Unset, Unset] = new FABuilder()

  //def withStates[S](s: )
}