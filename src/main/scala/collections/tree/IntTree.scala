package collections.tree


/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
class IntTree(x: Int) extends LazyTree[Int] {
  var children: Array[IntTree] = Array()

  def addChild(t: Int): Unit = {children = children :+ new IntTree(t)}
  def addChild(t: IntTree): Unit = {children = children :+ t}

  def getChild(d: Int): Option[IntTree] = {
    for(c <- children if c.data == d) return Some(c)
    None
  }
  override def data: Int = x
  override def generateChildren(): Seq[LazyTree[Int]] = children
  override def toString: String = "("+x+"):" + children.toString
}

object IntTree {
  /**
    * Generates a balanced binary search tree with n distinct numbers.
    * @param n The number of nodes in the generated tree
    * @param constant For internal use only; The value added to nodes
    *                 to ensure each is a different value.
    * @return The generated tree instance
    */
  def generate(n: Int, constant: Int = 0): IntTree = n match {
    case 1 => new IntTree(1 + constant)
    case 2 =>
      val t = new IntTree(1 + constant)
      t.addChild(2 + constant)
      t
    case _ =>
      val t = new IntTree((n / 2 + 1) + constant)
      // Left
      t.addChild(generate(n / 2, constant))
      // Right
      if(n%2==0)
        t.addChild(generate(n/2 - 1, (n/2 + 1) + constant))
      else
        t.addChild(generate(n/2, (n/2 + 1) + constant))
      t
  }
}