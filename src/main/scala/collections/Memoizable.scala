package collections

/**
  * Created by Dorian Thiessen on 2018-03-15.
  */
trait Memoizable[K, V] {
  def fromCache(key: K): V
}
