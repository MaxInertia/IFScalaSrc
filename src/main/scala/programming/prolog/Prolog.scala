package programming.prolog

import programming.Language

object Prolog extends Language {
  // Program consists of Facts and Rules
  trait Atom // Atoms start with lower-case letters
  trait Fact extends Atom // A clause with a head but no body
  trait Rule extends Atom // A clause with a head and a body
  type Term = Atom

  trait Variable { // Variables start with upper case letters
    def assign(value: Atom)
  }
  // No explicit quantifiers
  // Every variable is implicitly quantified using 'For all'

  // Declarative vs Procedural semantics...
  /* Declarative semantics for clauses
        The head of a clause is true if the body is true
          (if all properties or relationships in the body are true)
        Special case: A fact is taken to be true without any condition

     Procedural semantics for clauses
        To prove that the head of a clause is true, prove that the body is true
        Special case: A fact needs no proof

     Prolog proof procedure:
        To prove that a given goal is true:
        Find a clause whose head unifies with the goal,
        apply the mgu to the body of the clause,
        and then prove the body of the clause.
  */

  type Query //
  // Prolog only says: "here is an answer", not "here is THE answer"
  // It gives the first answer it finds, you have to tell it to find more

  /* Terms
      f(1)
      category(noun,plural)
        <- return true or false
      np(det(D),noun(cat),NP)
        <- return a binding for D and NP
   */

  // Singleton Variable: Variable with no conditions

  // UNIFICATION: The algorithm to 'match' terms (queries and facts) is called unification
  // Connected to Kruskal, Connected Components, etc

  object Interpreter extends Interpreter {
    def start(): Unit = execLoop()
    private def execLoop(): Unit = {

    }
  }
}
