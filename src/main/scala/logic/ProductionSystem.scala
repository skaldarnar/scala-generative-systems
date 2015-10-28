package logic

import collection.tree.{Leaf, Node, Tree}
import data._

import scala.util.Random

class ProductionSystem(val grammar: Grammar, val seed: Long) {

  import ProductionSystem._

  val rnd = new Random(seed)
  var root: Tree[Symbol] = new Node(grammar.start)

  def resetTree: Unit = root = new Node(grammar.start)

  def generate(): Tree[Symbol] = {
    rnd.setSeed(seed)
    val root: Tree[Symbol] = new Node(grammar.start)

    var openSymbols: List[Tree[Symbol]] = List(root)

    while (openSymbols.nonEmpty) {
      var next: List[Tree[Symbol]] = List()

      openSymbols.foreach {
        case n: Node[Symbol] => {
          val successors: List[Tree[Symbol]] = convList(selectProduction(n.v.asInstanceOf[Nonterminal]).toList)
          next ++= successors.filter(_.isInstanceOf[Node[Symbol]])
          n.cs = successors
        }
        case l: Leaf[Symbol] =>
        case _               =>
      }

      openSymbols = next
    }

    root
  }

  def derive(): Tree[Symbol] = {
    val openSymbols = root.openNodes
    openSymbols.foreach {
      case n: Node[Symbol] =>
        val successors: List[Tree[Symbol]] = convList(selectProduction(n.v.asInstanceOf[Nonterminal]).toList)
        n.cs = successors
      case _               =>
    }
    root
  }

  def derive(steps: Int): Tree[Symbol] = {
    for (_ <- 0 to steps) derive()
    root
  }

  def selectProduction(lhs: Nonterminal): Seq[Symbol] = { ps: Seq[Production] => ps(rnd.nextInt(ps.size)).rhs }.apply(findProductions(lhs))

  def findProductions(lhs: Symbol): Seq[Production] = grammar.productions.filter(_.lhs == lhs)
}

object ProductionSystem {

  implicit def terminalToTree(t: Terminal): Tree[Symbol] = Leaf(t)
  implicit def nonterminalToTree(n: Nonterminal): Tree[Symbol] = new Node(n)
  implicit def symbolToTree(s: Symbol): Tree[Symbol] = s match {
    case n: Nonterminal => nonterminalToTree(n)
    case t: Terminal => terminalToTree(t)
  }
  implicit def convList[S, T](in: List[S])(implicit c: S => T): List[T] = in map c

  implicit class RichNonterminal(s: Nonterminal) {
    def ->(lhs: Seq[data.Symbol]): Production = new Production(s, lhs)
  }

}
