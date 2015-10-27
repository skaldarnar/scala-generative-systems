package collection.tree

import scala.annotation.tailrec

sealed trait Tree[A] {

  var parent: Option[Tree[A]] = None

  def value: Option[A] = this match {
    case n: Node[A] => Some(n.v)
    case l: Leaf[A] => Some(l.v)
    case e: Eval[A] => Some(e.v)
  }

  def children: Option[Seq[Tree[A]]] = this match {
    case n: Node[A] => Some(n.cs)
    case l: Leaf[A] => None
    case _          => None
  }

  /**
   * Represents a deferred evaluation of a node value
   */
  private[this] case class Eval[A](v: A) extends Tree[A]

  /**
   * Represents common functionality of all traversal order folds
   *
   * @param a accumulator for (sub-)trees that are yet to fold
   * @param z initial folding value
   * @param f folding function, taking a tree value and the folded result
   * @param o a function that creates a new accumulator from a tree node and the list of remaining subtrees
   * @tparam A type of the tree's values
   * @tparam B type of the folding result
   * @return
   */
  @tailrec
  private def foldRec[A, B](a: List[Tree[A]], z: B)(f: (B, A) => B)(o: (Node[A], List[Tree[A]]) => List[Tree[A]]): B = a match {
    case (n: Node[A]) :: tl => foldRec(o(n, tl), z)(f)(o) // never directly evaluate nodes, function o will create new accumulator
    case (l: Leaf[A]) :: tl => foldRec(tl, f(z, l.v))(f)(o) // always evaluate Leaf
    case (e: Eval[A]) :: tl => foldRec(tl, f(z, e.v))(f)(o) // always evaluate Eval
    case _                  => z // will be Nil (empty list)
  }

  def foldPreorder[B](z: B)(f: (B, A) => B): B = {
    foldRec(List(this), z)(f) { (n, tl) => Eval(n.v) :: n.cs ::: tl }
  }

  def foldPostorder[B](z: B)(f: (B, A) => B): B = {
    foldRec(List(this), z)(f) { (n, tl) => n.cs ::: Eval(n.v) :: tl }
  }

  def foldLevelorder[B](z: B)(f: (B, A) => B): B = {
    foldRec(List(this), z)(f) { (n, tl) => (Eval(n.v) :: tl) ::: n.cs }
  }

  def fold[B](z: B)(f: (B, A) => B): B = foldPreorder(z)(f)

  def size: Int = fold(0) { (sum, _) => sum + 1 }

  def height: Int = {
    def rec(t: Tree[A]): Int = t match {
      case n: Node[A] => n.children.get.map(rec(_)).max + 1
      case l: Leaf[A] => 1
      case _          => 0
    }
    rec(this) - 1
  }

  def levels: List[List[A]] = {
    @tailrec
    def rec(lvl: List[Tree[A]], lvls: List[List[A]]): List[List[A]] = lvl match {
      case Nil              => lvls
      case l: List[Tree[A]] => rec(l.flatMap(_.children.getOrElse(Seq())), l.map(_.value.get) :: lvls)
    }
    rec(List(this), List()) reverse
  }

  def leafCount: Int = {
    foldRec(List(this), 0) { (sum: Int, v: A) => sum + 1 } { (n, tl) => n.cs ::: tl }
  }

  def leaves: List[A] = {
    foldRec(List(this), List[A]()) { (l: List[A], v: A) => v :: l } { (n, tl) => n.cs ::: tl } reverse
  }

  def toSeq: Seq[A] = fold(List[A]())((l, v) => v :: l) reverse

  def toSeqPreorder: Seq[A] = foldPreorder(List[A]())((l, v) => v :: l) reverse

  def toSeqPostorder: Seq[A] = foldPostorder(List[A]())((l, v) => v :: l) reverse

  def toSeqLevelorder: Seq[A] = foldLevelorder(List[A]())((l, v) => v :: l) reverse

  def level: Int = {
    @tailrec
    def rec(p: Option[Tree[_]], z: Int): Int = p match {
      case Some(t) => rec(t.parent, z+1)
      case None => z
    }
    rec(this.parent, 0)
  }

}

object Tree {
  implicit def nodeToTree[A](n: Node[A]): Tree[A] = n.asInstanceOf[Tree[A]]

  implicit def leafToTree[A](n: Leaf[A]): Tree[A] = n.asInstanceOf[Tree[A]]
}

case class Node[A](v: A, var cs: List[Tree[A]]) extends Tree[A] {

  cs.foreach(_.parent = Some(this))

  def this(v: A) = this(v, List())
}

case class Leaf[A](v: A) extends Tree[A]
