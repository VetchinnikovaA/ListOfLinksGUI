import java.util.function.Consumer

import scalafx.scene.canvas.Canvas
import scalafx.scene.input.{MouseDragEvent, MouseEvent}
import scalafx.scene.paint.Color
import scalafx.scene.text.Font

class ListRenderer[T](list: ListArray[T]) {
  var canvas: Canvas = _
  var viewx = 0
  var viewy = 0

  def render() = {
    var startx = 10  - viewx
    var x = startx
    var y = canvas.height.value / 2 - viewy
    var pos = 0
    var font_size = 30
    var gc = canvas.graphicsContext2D
    var dx = 50
    val draw = new Consumer[T] {
      override def accept(t: T) = {
        var text = t.toString
        var box_width = font_size * ( text.count((c:Char) => c.isLetter && c.toUpper.equals(c)) * 0.63 +
          text.count((c:Char) => c.isLetter && c.toLower.equals(c)) * 0.37 +
          text.count((c:Char) => c.isDigit) * 0.54)
        var box_height = font_size
        var border_size = 2
        gc.fill = Color.Black
        gc.fillRect(x - border_size - 1, y - border_size - box_height + font_size / 10, box_width + border_size * 2, box_height + border_size * 2)
        gc.fill = Color.AliceBlue
        gc.fillRect(x - 1, y - box_height + font_size / 10, box_width, box_height)
        gc.fill = Color.Black
        gc.font = Font(font_size)
        gc.fillText(text, x, y, box_width)

        var posstr = pos.toString
        var posx = x + (box_width - font_size * posstr.length * 0.54) / 2
        var posy = y - box_height
        gc.fill = Color.Black
        gc.fillRect(x - border_size - 1, posy - border_size - box_height + font_size / 10, box_width + border_size * 2, box_height + border_size * 2)
        gc.fill = Color.AliceBlue
        gc.fillRect(x - 1, posy - box_height + font_size / 10, box_width, box_height)
        gc.fill = Color.Black
        gc.font = Font(font_size)
        gc.fillText(posstr, posx, posy, box_width)

        if (x != startx) {
          var linky = y - box_height + border_size + 1
          var linkx = x - dx
          gc.fillRect(linkx, linky, dx, border_size)
        }

        pos += 1
        x += box_width.toInt + dx
      }}

    gc.fill = Color.AliceBlue
    gc.fillRect(0, 0, canvas.width.value, canvas.height.value)
    list.for_each(draw)
  }

  var prevx:Option[Double] = None
  var prevy:Option[Double] = None
  def dragged(event: MouseEvent) = {
    if (prevx.nonEmpty) {
      viewx -= event.x.toInt - prevx.get.toInt
      viewy -= event.y.toInt - prevy.get.toInt
    }
    prevx = Option(event.x)
    prevy = Option(event.y)
    render()
  }

  def released() = {
    prevx = None
    prevy = None
  }
}
