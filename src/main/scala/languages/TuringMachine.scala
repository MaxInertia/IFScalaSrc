package languages

import languages.TurningMachine.Tape

import scala.collection.mutable

/**
  * @param input The input tape
  */
class TuringMachine(private var input: mutable.MutableList[Char]) {
  val tape: Tape = new Tape(input)
  class Tape(private var content: mutable.MutableList[Char]) {
    private var pos: Int = 0
    def getCell: Char = content(pos)
    def moveRight(): Unit = {
      if (pos >= content.length) content :+ Tape.blankCell // Append a blank cell
      pos += 1 // Increment pos
    }
    def moveLeft(): Unit =
      if(pos == 0) Tape.blankCell +: content // Prepend a blank cell
      else pos -= 1 // Otherwise decrement pos
  }
}

/**
  * @param input The input tape
  */
class MultiTapeTM(input: mutable.MutableList[Char], numTapes: Int) extends TuringMachine(input) {
  val tapes: Array[Tape] = {
    var arr: Array[Tape] = Array(new Tape(input))
    for(_ <- 0 until numTapes) arr =
      arr :+ new Tape(mutable.MutableList[Char](Tape.blankCell))
    arr
  }
}

object TurningMachine {
  object Tape {
    val blankCell: Char = ' '
  }
}

/* Power Heirarchy
    Deterministic TM (Same as NonDeterministic TM)
    > CFL
    > DCFL
    > Regular Languages (DFA, NFA, e_NFA)
 */

/*
If you can convince yourself you can't do something in polynomial time on a Turing Machine,
then you can also be convinced that it cannot be done in PT on a random access machine
 */

object Main extends App {

}