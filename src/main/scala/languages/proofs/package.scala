package languages

package object proofs {
  trait Proof

  /**
    * To prove a language L is not regular
    * 1) Assume L is regular
    * 2) Apply the Pumping Lemma
    */
  trait PumpingLemma extends Proof

}
