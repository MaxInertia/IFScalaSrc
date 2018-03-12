import collections.tree.{IntTree, LazyTree}
import com.sun.org.apache.xpath.internal.operations.Bool
import operations.search.TreeTraversal

/**
  * Created by Dorian Thiessen on 2018-03-10.
  */
object Main extends App {

  def treeTests(): Unit = {
    val tree = new IntTree(4)
    tree.addChild(2)
    tree.addChild(6)
    println(s"tree: ${tree.children.length}")
    println(tree)
    val t2 = tree.getChild(2)
    val t6 = tree.getChild(6)
    if(t2.nonEmpty) {
      t2.get.addChild(1)
      t2.get.addChild(3)
      println(s"t2: ${t2.get.children.length}")
    }
    if(t6.nonEmpty) {
      t6.get.addChild(5)
      t6.get.addChild(7)
      println(s"t4: ${t6.get.children.length}")
    }

    val eval: Int => Boolean = (x: Int) => {
      print(x +" ")
      false
    }

    println()
    TreeTraversal.dfs(tree)(eval)
    println()
    TreeTraversal.bfs(tree)(eval)



    println()
    val t8 = IntTree.generate(8)
    TreeTraversal.bfs(t8)(eval)
    println()
    TreeTraversal.dfs(t8)(eval)
    println()
  }

  val tree = IntTree.generate(200)
  val evaluator: Int => Double = (x: Int) => x.toDouble
  val evaluatorWPrint: Int => Double = (x: Int) => {
    print(x + ", ")
    x.toDouble
  }
  val isTerminal: Int => Boolean = _ => false
  val isTerminalWPrint: Int => Boolean = (x: Int) => {
    print(x +", ")
    false
  }

  class CallCounter {
    var n = 0
    def increment(): Unit = n+=1
    def applyThenInc[A,B](f: A => B)(in: A): B = {
      increment()
      f(in)
    }
  }

  def compareMMwithAB(intTreeSize: Int, depthLimit: Int): Unit = {
    val t = IntTree.generate(intTreeSize)

    val mmC = new CallCounter
    val best1 = TreeTraversal.minimax(t, depthLimit)(evaluator, mmC.applyThenInc(isTerminal))
    println("MM: "+ best1)

    val abC = new CallCounter
    val ab = TreeTraversal.alphabeta(evaluator, abC.applyThenInc(isTerminal)) _
    println("AB: "+ ab(t, depthLimit, true))

    println(s"mmC: ${mmC.n}\nabC: ${abC.n}\n")
  }

  compareMMwithAB(1,1)
  compareMMwithAB(2,1)
  compareMMwithAB(4,1)
  compareMMwithAB(8,2)
  compareMMwithAB(16,3)
  compareMMwithAB(32,4)
  compareMMwithAB(64,5)
  compareMMwithAB(128,6)
  compareMMwithAB(256,7)
  compareMMwithAB(512,8)
  compareMMwithAB(1024,9)
}

