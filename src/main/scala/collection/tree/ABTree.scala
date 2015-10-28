package collection.tree

import scala.annotation.tailrec

sealed trait ABTree[A, B] {

  def value: Either[A, B] = this match {
    case ABNode(v, _) => Left(v)
    case ABLeaf(v)    => Right(v)
  }

  def children: Option[List[ABTree[A, B]]] = this match {
    case ABNode(_, cs) => Some(cs)
    case ABLeaf(_)     => None
    case _             => None
  }

  private[this] case class ABEval[A, B](v: Either[A, B]) extends ABTree[A, B] {
    override def value: Either[A, B] = v
  }

  @tailrec
  private def foldRec[A, B, C](a: List[ABTree[A, B]], z: C)
                              (f: (C, Either[A, B]) => C)
                              (o: (ABNode[A, B], List[ABTree[A, B]]) => List[ABTree[A, B]]): C = a match {
    case (n: ABNode[A, B]) :: ts => foldRec(o(n, ts), z)(f)(o)
    case (l: ABLeaf[A, B]) :: ts => foldRec(ts, f(z, l.value))(f)(o)
    case (e: ABEval[A, B]) :: ts => foldRec(ts, f(z, e.value))(f)(o)
    case _                       => z
  }

  def foldPreorder[C](z: C)(f: (C, Either[A, B]) => C): C = {
    foldRec(List(this), z)(f) { (n, tl) => ABEval(n.value) :: n.cs ::: tl }
  }

  def foldPostorder[C](z: C)(f: (C, Either[A, B]) => C): C = {
    foldRec(List(this), z)(f) { (n, tl) => n.cs ::: ABEval(n.value) :: tl }
  }

  def foldLevelorder[C](z: C)(f: (C, Either[A, B]) => C): C = {
    foldRec(List(this), z)(f) { (n, tl) => (ABEval(n.value) :: tl) ::: n.cs }
  }

  def fold[C](z: C)(f: (C, Either[A, B]) => C): C = foldPreorder(z)(f)
}

case class ABNode[A, B](v: A, cs: List[ABTree[A, B]]) extends ABTree[A, B]

case class ABLeaf[A, B](v: B) extends ABTree[A, B]