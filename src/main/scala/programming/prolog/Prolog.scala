package programming.prolog

import programming.{Language, prolog}

object Prolog extends Language {

  // Program consists of Facts and Rules
  trait Atom { // Atoms start with lower-case letters
    def toString: String
  }
  //trait Fact extends Atom // A clause with a head but no body
  class Fact(head: String) extends Atom{
    override def toString: String = head
  }
  //trait Rule extends Atom // A clause with a head and a body
  // Rule declares a relationship
  // squared(X,Y) :- X is Y^2
  class Rule(head: String, body: String) extends Atom {
    override def toString: String = head +" : "+ body
  }
  type Term = Atom

  // Variables start with upper case letters
  // No explicit quantifiers
  // Every variable is implicitly quantified using 'For all'
  trait Variable {
    def assign(value: Atom)
  }

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

  type Query
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

  // Comments: %, Multi-line comment: %%
  class Context(val terms: List[Atom] = List()) {
    def add(x: Atom): Context = new Context(x +: terms)
  }

  object Interpreter extends Interpreter {
    def start(): Unit = execLoop(new Context())
    private def execLoop(ctx: Context): Unit = {
      print("?- ")
      val input = scala.io.StdIn.readLine()
      if (input == "show") {
        show(ctx)
        execLoop(ctx)
      } else {
        val atom = input2atom(input)
        val updatedCtx: Context = ctx.add(atom)
        execLoop(updatedCtx)
      }
    }
  }
  private def input2atom(input: String): Atom = new Fact(input)
  private def show(ctx: Context): Unit = {
    println("Context content: ")
    for(c <- ctx.terms) println("\t" + c)
  }
}

object Main extends App {
  val pi = Prolog.Interpreter
  pi.start()
}

/*
- Syntax: Horn Clause Logic
- Inference: Modus Ponens via backward chaining and backtracking
  1. Unification: A tool for matching terms
  2. Resolution: An inference procedure for Prolog

  A retricted form of First Order Predicate Calculus
  A Horn Clause:
    1. A fact
    2. A rule with:
      - One goal as the head of the rule
      - The symbol :-
      - A sequence of sub-goals, separated by commas
  A fact can be seen as a rule with no body

  UNIFICATION:
    - A sophisticated form of matching
    - Intuition:
      - If I want to prove a goal G, can i use rule R?
      - Yes, if G unifies with the head of rule R.
*/