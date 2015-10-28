package data

class Grammar(val productions: Seq[Production]) {

  val start: Nonterminal = productions.map(_.lhs).head

  val terminals: Set[Terminal] = productions.foldLeft(Set[Terminal]())((result, p) =>
    result ++ p.rhs.filter(_.isInstanceOf[Terminal]).map(_.asInstanceOf[Terminal]))

  val nonterminals: Set[Nonterminal] = productions.foldLeft(Set[Nonterminal]())((result, p) => (result + p.lhs) ++ p.rhs.filter(_.isInstanceOf[Nonterminal]).map(_.asInstanceOf[Nonterminal]))

  override def toString = {
    val builder = new StringBuilder()

    builder ++= "G = (N, T, P, S) with \n"

    builder ++= "N = {"
    builder ++= nonterminals.mkString(",")
    builder ++= "} \n"

    builder ++= "T = {"
    builder ++= terminals.mkString(",")
    builder ++= "} \n"

    builder ++= "S = " + start + "\n"

    builder ++= "P = { \n"
    builder ++= productions.foldLeft("")((s, p) => s + "\t" + p + "\n")
    builder ++= "}"

    builder.toString()
  }
}
