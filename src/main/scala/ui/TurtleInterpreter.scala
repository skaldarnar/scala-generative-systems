package ui

import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color

class TurtleInterpreter(var d: Double = 1.0, var a: Double = math.toRadians(90)) {

  private var t = (0.0, 0.0, 0.0)

  def position = (t._1, t._2)

  def position_=(p: (Double, Double)): Unit = t = (p._1, p._2, t._3)

  def angle = t._3

  def angle_=(a: Double): Unit = t = (t._1, t._2, a)

  def state = t

  def state_=(s: (Double, Double, Double)): Unit = t = s

  def interpret(word: Seq[String], gc: GraphicsContext) = {
    gc.setFill(Color.BLACK)
    gc.setLineWidth(1)
    gc.moveTo(t._1, t._1)

    word.foreach {
      case x if x.startsWith("F") =>
        val old = t
        t = (t._1 + d * math.cos(t._3), t._2 + d * math.sin(t._3), t._3)
        gc.strokeLine(old._1, old._2, t._1, t._2)
      case "+"                    => t = (t._1, t._2, t._3 + a)
      case "-"                    => t = (t._1, t._2, t._3 - a)
    }
  }

}
