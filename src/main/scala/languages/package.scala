import collections.{Defines, Implicitly}

package object languages {

  type Word = String

  // Phantom Types
  sealed trait Type
  trait Unknown extends Type  // May not know if some language L is regular
  trait Regular extends Type
  trait NonRegular extends Type

  trait Language[T <: Type] {
    def contains(w: Word): Boolean
  }

  /*implicit object DFALanguage extends Defines[DeterministicFA, Implicitly, Language[Regular]] {
    override def produce(a: DeterministicFA): Language[Regular] = ??? // TODO: Implement DFA->Lang
  }
  implicit object NFALanguage extends Defines[NonDeterministicFA, Implicitly, Language[Regular]] {
    override def produce(a: NonDeterministicFA): Language[Regular] = ??? // TODO: Implement NFA->Lang
  }
  implicit object EpsilonNFALanguage extends Defines[EpsilonNFA, Implicitly, Language[Regular]] {
    override def produce(a: EpsilonNFA): Language[Regular] = ??? // TODO: Implement eNFA->Lang
  }*/

  object Language {
    // How do I say...
    // For 'A' and 'B' such that
    // there is an implicit instance of
    // Defines[A, _, B] ???
    def from[A, B](a: A): B =  ??? /*{
      lazy val converter = implicitly[Defines[A, _, B]]
      converter.produce(a)
    }*/
  }
}
