package object languages {

  type Word = String

  // Phantom Type
  sealed trait Type
  trait Unknown extends Type  // May not know if some language L is regular
  trait Regular extends Type
  trait NonRegular extends Type

  trait Language[T <: Type] {
    def contains(w: Word): Boolean
  }

  //trait RegularLanguage extends Language
  //trait NonRegularLanguage extends Language

  // A implicitly or explicitly defines a discrete instance of B
  // - 'A' can produce a 'B'
  // - 'A' can be used in place of a 'B'
  trait Defines[A, B] {
    def produce(a: A): B

  }

  /*implicit object DFADefinesLang extends Defines[DeterministicFA, Language[Regular]]
  implicit object NFADefinesLang extends Defines[NonDeterministicFA, Language[Regular]]
  implicit object EpsilonNFADefinesLang extends Defines[EpsilonNFA, Language[Regular]]*/

}

