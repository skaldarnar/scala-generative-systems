package collection.tree.bench

import data.{Grammar, Terminal, Nonterminal}
import logic.ProductionSystem
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 20, time = 4, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class TreeBench {

  val S = new Nonterminal("S")
  val A = new Nonterminal("A")
  val B = new Nonterminal("B")
  val C = new Nonterminal("C")

  val t = new Terminal("t")
  val x = new Terminal("x")
  val y = new Terminal("y")
  val z = new Terminal("z")

  val g = new Grammar(Seq(
    S -> Seq(A, B, A, B),
    A -> Seq(t, A, x),
    A -> Seq(t, A, x),
    A -> Seq(t, A, x),
    A -> Seq(x, x),
    A -> Seq(C),
    B -> Seq(B, C, C, B),
    B -> Seq(y, z, B),
    B -> Seq(y, z, B),
    B -> Seq(C, A, C),
    C -> Seq(y, x, y),
    C -> Seq(t, x, t),
    C -> Seq(z, x, z)
  ))

  val sys = new ProductionSystem(g, "asdf".hashCode)

  val tree = sys.generate3()

  @Benchmark
  def benchLeafCount(): Unit = {
    tree.leafCount
  }

  @Benchmark
  def benchLeafCount2(): Unit = {
    tree.leafCount2
  }

  @Benchmark
  def benachLeaves(): Unit = {
    tree.leaves
  }

  @Benchmark
  def benchLeaves2(): Unit = {
    tree.leaves2
  }




}
