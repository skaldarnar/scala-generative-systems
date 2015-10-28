package ui

import javafx.event.EventHandler
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent, ScrollEvent}
import javafx.scene.{Node, Scene}

class Camera(var pos: (Double, Double) = (0, 0),
             var zoom: Double = 1.0) {

  def translate(dx: Double, dy: Double): Unit = {
    pos = (pos._1 + dx / zoom, pos._2 + dy / zoom)
  }

}

object Camera {
  def cameraKeyController(cam: Camera): EventHandler[KeyEvent] = new EventHandler[KeyEvent] {
    val moveInterval = 8

    override def handle(event: KeyEvent): Unit = event.getCode match {
      case KeyCode.W | KeyCode.UP    => cam.translate(0, -moveInterval)
      case KeyCode.S | KeyCode.DOWN  => cam.translate(0, +moveInterval)
      case KeyCode.A | KeyCode.LEFT  => cam.translate(-moveInterval, 0)
      case KeyCode.D | KeyCode.RIGHT => cam.translate(+moveInterval, 0)
      case _                         =>
    }
  }

  def cameraMouseController(cam: Camera): EventHandler[MouseEvent] = new EventHandler[MouseEvent] {
    var last = (0.0, 0.0)

    override def handle(event: MouseEvent): Unit = event.getEventType match {
      case MouseEvent.MOUSE_PRESSED => last = (event.getSceneX, event.getSceneY)
      case MouseEvent.MOUSE_DRAGGED =>
        cam.translate((event.getSceneX - last._1) * cam.zoom, (event.getSceneY - last._2) * cam.zoom)
        last = (event.getSceneX, event.getSceneY)
      case _                        =>
    }
  }

  def cameraScrollController(cam: Camera): EventHandler[ScrollEvent] = new EventHandler[ScrollEvent] {
    var zoomLevel: Int = findZoomLevel(cam.zoom)
    val minZoomLevel = -32
    val maxZoomLevel = 8
    val zoomDelta = 0.25

    def findZoomLevel(zoom: Double) = math.round((math.log(zoom) / math.log(2)) / zoomDelta).toInt

    override def handle(event: ScrollEvent): Unit = {
      zoomLevel += math.round(event.getDeltaY / event.getMultiplierY).toInt
      zoomLevel = if (zoomLevel < minZoomLevel) minZoomLevel else if (zoomLevel > maxZoomLevel) maxZoomLevel else zoomLevel

      val zoom = math.pow(2.0, zoomLevel * zoomDelta)

      val scene: Scene = event.getSource match {
        case n: Node  => n.getScene
        case s: Scene => s
      }

      val rel: (Double, Double) = (event.getSceneX - scene.getWidth / 2, event.getSceneY - scene.getHeight / 2)

      cam.translate(rel._1, rel._2)
      cam.zoom = zoom
      cam.translate(-rel._1, -rel._2)
    }
  }
}
