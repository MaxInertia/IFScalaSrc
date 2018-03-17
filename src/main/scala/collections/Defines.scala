package collections

// Created by Dorian Thiessen on 2018-03-14.

// Phantom types
sealed trait Type
trait Explicitly extends Type
trait Implicitly extends Type

/**
  * A implicitly or explicitly defines a single instance of B
  * - 'A' can produce a 'B'
  * - 'A' can be used in place of a 'B' // TODO: Lazily produce a B, calling the required method on it?
  * Explicitly: 'A' stores an instance of 'B' that is returned (Minimal work)
  * Implicitly: Work must be done by 'A' to generate an instance of 'B'
  */
trait Defines[A, T <: Type, B] {
  def produce(a: A): B
}
//val y: DeterministicFA Defines Explicitly Language Regular = null
//val z: Defines[DeterministicFA, Explicitly, Language[Regular]] = null

