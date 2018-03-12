package operations.search

/**
  * Created by Dorian Thiessen on 2018-03-10.
  *
  * N: Node class
  */
trait TreeSearch[N] {
  def search(): Option[N]
}