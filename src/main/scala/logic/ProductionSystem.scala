package logic

import collection.tree.{Leaf, Node, Tree}
import data._

import scala.util.Random

class ProductionSystem(grammar: Grammar, seed: Long) {

  val rnd = new Random(seed)

  def generate3(): Tree[Symbol] = {
    rnd.setSeed(seed)
    val root: Tree[Symbol] = new Node(grammar.start)

    var openSymbols: List[Tree[Symbol]] = List(root)

    while (openSymbols.nonEmpty) {

      implicit def terminalToTree(t: Terminal): Tree[Symbol] = Leaf(t)
      implicit def nonterminalToTree(n: Nonterminal): Tree[Symbol] = new Node(n)
      implicit def symbolToTree(s: Symbol): Tree[Symbol] = s match {
        case n: Nonterminal => nonterminalToTree(n)
        case t: Terminal => terminalToTree(t)
      }
      implicit def convList[S, T](in: List[S])(implicit c: S => T): List[T] = in map c

      var next: List[Tree[Symbol]] = List()

      openSymbols.foreach(_ match {
        case n: Node[Symbol] => {
          val successors: List[Tree[Symbol]] = convList(selectProduction(n.value.get.asInstanceOf[Nonterminal]).toList)
          next ++= successors.filter(_.isInstanceOf[Node[Symbol]])
          n.cs = successors
        }
        case l: Leaf[Symbol] =>
        case _ =>
      })

      openSymbols = next
    }

    root
  }

  def generate2(): Symbol = {
    var current = Seq[Symbol](grammar.start)

    while(current.nonEmpty) {
      var next = Seq[Symbol]()
      current.foreach {
        case t:Terminal => Unit
        case n:Nonterminal => {
          val successors = selectProduction(n)
          n.setChildren(successors)
          next ++= successors
        }
      }
      current = next
    }

    grammar.start
  }

  def generate(): List[Seq[Symbol]] = {
    rnd.setSeed(seed)

    var current = Seq[Symbol](grammar.start)
    var history = List[Seq[Symbol]]()

    while (current.exists(_.isInstanceOf[Nonterminal])) {
      history ::= current

      current = current.flatMap {
        case t: Terminal => Seq[Symbol](t)
        case n: Nonterminal => selectProduction(n)
      }
    }

    current :: history
  }

  def selectProduction(lhs: Nonterminal): Seq[Symbol] = { ps: Seq[Production] => ps(rnd.nextInt(ps.size)).rhs }.apply(findProductions(lhs))

  def findProductions(lhs: Symbol): Seq[Production] = grammar.productions.filter(_.lhs == lhs)

}
