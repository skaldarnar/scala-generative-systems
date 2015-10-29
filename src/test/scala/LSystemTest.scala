import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.application.Application
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.Scene
import javafx.scene.input._
import javafx.scene.layout.Pane
import javafx.stage.Stage
import javafx.util.Duration

import collection.tree.{Node, Tree}
import data.{Grammar, Nonterminal, Symbol, Terminal}
import logic.ProductionSystem
import logic.ProductionSystem._
import ui.{Camera, ResizableCanvas, TurtleInterpreter}

object LSystemTest {

  def main(args: Array[String]) {
    Application.launch(classOf[LSystemTest], args: _*)
  }
}

class LSystemTest extends Application {
  val S = new Nonterminal("S")
  val Fl = new Nonterminal("Fl")
  val Fr = new Nonterminal("Fr")
  val F = new Nonterminal("F")
  val right = new Terminal("-")
  val left = new Terminal("+")

  def dragonCurve: Grammar = new Grammar(Seq(
    S -> Seq(Fl),
    Fl -> Seq(Fl, left, Fr, left),
    Fr -> Seq(right, Fl, right, Fr)
  ))

  def kochIsland = new Grammar(Seq(
    S -> Seq(F, right, F, right, F, right, F),
    F -> Seq(F, right, F, left, F, left, F, F, right, F, right, F, left, F)
  ))

  override def start(primaryStage: Stage): Unit = {
    val grammar = dragonCurve

    var dt: Tree[Symbol] = new Node(grammar.start)

    val sys = new ProductionSystem(grammar, 0)

    val canvas = new ResizableCanvas()
    val root = new Pane(canvas)
    canvas.widthProperty().bind(root.widthProperty())
    canvas.heightProperty().bind(root.heightProperty())

    val scene = new Scene(root, 300, 300)

    val gc = canvas.getGraphicsContext2D

    val camera = new Camera((canvas.getWidth / 2, canvas.getHeight / 2))
    scene.addEventHandler(KeyEvent.KEY_PRESSED, Camera.cameraKeyController(camera))
    scene.addEventHandler(MouseEvent.ANY, Camera.cameraMouseController(camera))
    scene.addEventHandler(ScrollEvent.ANY, Camera.cameraScrollController(camera))

    var derivationSteps = 2
    var lastDerivationSteps = derivationSteps
    var segmentLength = 100.0
    var word: Seq[String] = Seq()
    dt = sys.derive(derivationSteps)
    word = dt.collect(t => t.children.isEmpty || t.children.get.isEmpty).map(_.value.get.id)

    val turtle = new TurtleInterpreter(segmentLength, math.toRadians(90))
    turtle.state = (camera.pos._1, camera.pos._2, math.toRadians(90))

    scene.addEventHandler(KeyEvent.KEY_PRESSED, new EventHandler[KeyEvent] {
      override def handle(event: KeyEvent): Unit = {
        event.getCode match {
          case KeyCode.PLUS | KeyCode.ADD       => derivationSteps += 1
          case KeyCode.MINUS | KeyCode.SUBTRACT => derivationSteps = math.max(0, derivationSteps - 1)
          case _                                =>
        }
        event.consume()
      }
    })

    primaryStage.setScene(scene)
    primaryStage.show()

    val timeline = new Timeline()
    timeline.setCycleCount(Animation.INDEFINITE)
    timeline.getKeyFrames.add(new KeyFrame(Duration.millis(40), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        gc.clearRect(0, 0, canvas.getWidth, canvas.getHeight)

        gc.strokeLine(0, canvas.getHeight / 2, canvas.getWidth, canvas.getHeight / 2)
        gc.strokeLine(canvas.getWidth / 2, 0, canvas.getWidth / 2, canvas.getHeight)

        turtle.position = camera.pos
        turtle.d = segmentLength * camera.zoom
        turtle.angle = 0

        if (lastDerivationSteps != derivationSteps) {
          sys.resetTree
          dt = sys.derive(derivationSteps)
          word = dt.collect(t => t.children.isEmpty || t.children.get.isEmpty).map(_.value.get.id)
          turtle.d = segmentLength
          lastDerivationSteps = derivationSteps
        }

        turtle.interpret(word, gc)
      }
    }))
    timeline.play()
  }
}