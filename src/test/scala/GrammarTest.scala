import data.{Grammar, Nonterminal, Terminal}
import logic.ProductionSystem

object GrammarTest {
  def main(args: Array[String]) {
    val S = new Nonterminal("S")
    val A = new Nonterminal("A")
    val B = new Nonterminal("B")
    val C = new Nonterminal("C")

    val t = new Terminal("t")
    val x = new Terminal("x")
    val y = new Terminal("y")
    val z = new Terminal("z")

    val g = new Grammar(Seq(
      S -> Seq(A, B, A),
      A -> Seq(t, A, x),
      A -> Seq(x,x),
      A -> Seq(C),
      B -> Seq(z),
      B -> Seq(y, z, B),
      C -> Seq(y, x, y),
      C -> Seq(t, x, t),
      C -> Seq(z, x, z)
    ))

    println("Grammar:")
    println(g)

    var sys = new ProductionSystem(g, "asdfasdf".hashCode)

    println("\n\n")
    println("history keeping approach")
    val hist = sys.generate()
    (hist reverse) foreach println

    println("\n\n")
    println("tree approach")
    sys = new ProductionSystem(g, "asdfasdf".hashCode)
    (sys.generate3() levels) foreach println

  }
}
