package collection.tree.bench

import java.util.concurrent.TimeUnit

import data.{Grammar, Nonterminal, Terminal}
import logic.ProductionSystem
import logic.ProductionSystem._
import org.openjdk.jmh.annotations._

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

  val tree = sys.generate()

  @Benchmark
  def benchLeafCount(): Unit = {
    tree.leafCount
  }

  @Benchmark
  def benchLeaves(): Unit = {
    tree.leaves
  }
}
