package data

class Production(val lhs: Nonterminal, val rhs: Seq[Symbol]) {

    override def toString = lhs + " -> " + rhs.mkString("")
}
