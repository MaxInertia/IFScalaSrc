package collections.tree

import scala.collection.mutable

/**
  * Created by Dorian Thiessen on 2018-03-15.
  */
object Experimental extends App {
  /*trait Type
  object Verbose extends Type
  object Minimal extends Type

  def runSearch[T <: Type](treeSize: Int, depthLimit: Int, t: Type = Minimal): Unit = {
    val isTerminal: Int => Boolean = _ => false
    var count = 0
    val counter: () => Unit = () => {count += 1}

    if(t == Verbose) {
      println(s"Tree size: $treeSize nodes")
      println(s"Depth limit: $depthLimit")
    }
    val tree: LazyTree[Int] = IntTree.generate(treeSize)

    TreeSearch.miniMax((x: Int) => x, isTerminal, counter)(tree, depthLimit)
    if(t == Verbose) {
      println(s"\tMiniMax searched $count nodes\n")
    } else {
      println(s"mm($treeSize, $depthLimit) = $count")
    }

    count = 0

    TreeSearch.alphaBeta((x: Int) => x, isTerminal, counter)(tree, depthLimit)
    if(t == Verbose) {
      println(s"\tAlphaBeta searched $count nodes")
    } else {
      println(s"ab($treeSize, $depthLimit) = $count\n")
    }
  }
  runSearch(16, 4)
  runSearch(32, 4)
  runSearch(64, 1)
  runSearch(64, 2)
  runSearch(64, 3)
  runSearch(64, 4)
  runSearch(128, 4)
  runSearch(256, 4)*/

  // TODO: Add branching factor as a variable

  type Inputs = (Int, Int)
  type Outputs = (Int, Int)

  def search(treeSize: Int, depthLimit: Int): Outputs = {
    var count = 0
    val counter: () => Unit = () => {count += 1}
    val isTerminal: Int => Boolean = _ => false

    val tree: LazyTree[Int] = IntTree.generate(treeSize)

    val mmVal = TreeSearch.miniMax((x: Int) => x, isTerminal, counter)(tree, depthLimit)
    val mmNodes = count
    count = 0
    val abVal = TreeSearch.negamaxAlphaBeta((x: Int) => x, isTerminal, counter)(tree, depthLimit, 1, -Double.MaxValue, Double.MaxValue)
    val abNodes = count
    assert(mmVal == abVal, s"(MiniMax value: $mmVal) != (AlphaBeta value: $abVal)")
    (mmNodes, abNodes)
  }
  def varyDepthLimit(nodes: Int, range: Range.Inclusive): mutable.LinkedHashMap[Inputs, Outputs] = {
    var stats: mutable.LinkedHashMap[Inputs, Outputs] = mutable.LinkedHashMap()
    for{i <- range} {
      stats += (nodes, i) -> search(nodes, i)
    }
    stats
  }

  def printStats(stats: mutable.LinkedHashMap[Inputs, Outputs])(implicit colSize: Int): Unit = {
    printf("%s|%s|%s|%s\n%s\n",
      prettyString("nodes"),
      prettyString("dlim"),
      prettyString("mm"),
      prettyString("ab"),
      prettyString("", '-')(colSize*4 + 3))

    for(s <- stats) {
      val ((n, d), (mm, ab)) = s
      printf("%s|%s|%s|%s\n",
        prettyString(n),
        prettyString(d),
        prettyString(mm),
        prettyString(ab))
    }
  }

  def prettyString[T](n: T, fill: Char = ' ')(implicit len: Int): String = {
    var str = " " + n.toString
    while(str.length < len) {
      str += fill
    }
    str
  }

  implicit val colSize: Int = 7
  for(i <- 17 to 20) {
    val stats = varyDepthLimit(math.pow(2, i).toInt, 1 to i+1)
    printStats(stats)
    printf("%s\n", prettyString("", '-')(colSize*4 + 3))
  }

}
