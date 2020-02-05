import java.io.PrintStream
import java.lang.String
import java.lang.StringBuilder
import java.util.function.Consumer
import java.util.{Comparator, Random}
import javafx.collections.ObservableList
import java.io
import java.io.OutputStream

import scalafx.Includes._
import scalafx.application
import scalafx.application.JFXApp
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.{Cursor, Node, Scene, canvas}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, TextArea, TextField, TextInputDialog}
import scalafx.scene.layout.{Background, BorderPane, FlowPane, Pane}
import scalafx.scene.paint.{Color, Paint}
import scalafx.stage.StageStyle
import scalafx.scene.input.{MouseDragEvent, MouseEvent}
import javax.swing.JPanel
import javax.swing.JFrame
import javax.swing.JTextArea
import javax.swing.JScrollPane
import java.awt.BorderLayout

//remove if not needed
import scala.collection.JavaConversions._

object Main extends JFXApp {
  val mprint = new Consumer[String] {
    override def accept(t: String) = System.out.print(t + " ")}
  stage = new application.JFXApp.PrimaryStage {
    var controls: FlowPane = _
    var console: TextArea = new TextArea() {
               maxWidth = 400
                maxHeight = 600
    }
    console.setEditable(false)
    var list = new ListArray[String]
    var canvas: Canvas = new Canvas() {
      layoutX = 0
      layoutY = 0
      width = 400
      height = 600
      this.graphicsContext2D.fill = Color.AliceBlue
      this.graphicsContext2D.fillRect(0, 0, width.value, height.value)
    }
    title.value = "LR3"
    resizable = false
    scene = new Scene {
      root = new BorderPane() {
        controls = new FlowPane() {
          maxWidth = 400
          children = List(new Pane() {
            children = List(new TextField() {
              layoutX = 10
              layoutY = 25
              maxWidth = 75
              id = "AddValue"
              promptText = "Значение"
            }, new Button() {
              layoutX = 90
              layoutY = 25
              text = "Добавить в конец"
              onAction = (event: ActionEvent) => {
                var AddValue = children.filter((n: javafx.scene.Node) => { "AddValue".equals(n.id.value) } )
                var text = AddValue.get(0).asInstanceOf[javafx.scene.control.TextField].getText
                if (!text.isEmpty) {
                  list.add(text)
                  console.setText(null)
                  console.appendText(list.printConsole() + "\n")
                }
              }
            }, new TextField() {
              layoutX = 10
              layoutY = 65
              maxWidth = 75
              id = "AddIdxValue"
              promptText = "Значение"
            }, new TextField() {
              layoutX = 90
              layoutY = 65
              maxWidth = 75
              id = "AddIdxIdx"
              promptText = "Индекс"
            }, new Button() {
              layoutX = 170
              layoutY = 65
              text = "Добавить по индексу"
              onAction = (event: ActionEvent) => {
                var addIdxValue = children.filter((n: javafx.scene.Node) => { "AddIdxValue".equals(n.id.value) } )
                var addIdxIdx = children.filter((n: javafx.scene.Node) => { "AddIdxIdx".equals(n.id.value) } )
                var text = addIdxValue.get(0).asInstanceOf[javafx.scene.control.TextField].getText
                var idx_text = addIdxIdx.get(0).asInstanceOf[javafx.scene.control.TextField].getText
                if (!idx_text.isEmpty) {
                  var idx = idx_text.toInt
                  if (!text.isEmpty) {
                    list.insert(text, idx)
                    console.setText(null)
                    console.appendText(list.printConsole() + "\n")
                  }
                }
              }
            }, new TextField() {
              layoutX = 10
              layoutY = 105
              maxWidth = 75
              id = "getIdx"
              promptText = "Индекс"
            }, new Button() {
              layoutX = 90
              layoutY = 105
              text = "Получить по индексу"
              onAction = (event: ActionEvent) => {
                  var getIdx = children.filter((n: javafx.scene.Node) => { "getIdx".equals(n.id.value) } )
                  var idx_text = getIdx.get(0).asInstanceOf[javafx.scene.control.TextField].getText
                  if (!idx_text.isEmpty) {
                    var idx = idx_text.toInt
                    console.setText(null)
                    console.appendText(list.printConsole() + "\n")
                    console.appendText(list.get(idx)+ "\n")
                  }
              }
            }, new TextField() {
              layoutX = 10
              layoutY = 145
              maxWidth = 75
              id = "DelIdxIdx"
              promptText = "Индекс"
            }, new Button() {
              layoutX = 90
              layoutY = 145
              text = "Удалить по индексу"
              onAction = (event: ActionEvent) => {
                var delIdxIdx = children.filter((n: javafx.scene.Node) => { "DelIdxIdx".equals(n.id.value) } )
                var idx_text = delIdxIdx.get(0).asInstanceOf[javafx.scene.control.TextField].getText
                if (!delIdxIdx.isEmpty) {
                  var idx = idx_text.toInt
                  list.remove(idx)
                  console.setText(null)
                  console.appendText(list.printConsole() + "\n")
                }
              }
            }, new Button() {
              layoutY = 185
              layoutX = 10
              text = "Отсортировать список"
              onAction = (event: ActionEvent) => {
                list.sort(false)
                console.setText(null)
                console.appendText(list.printConsole() + "\n")
              }
            })
          }
          )
        }
        left = controls
        right = console
      }
    }
    /*def generateInt: Int = {
    val count = (Math.random * 30).toInt
    count
  }

  def generateString: String = {
    val symbols = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdyfghijklmnopqrstuvwxyz1234567890 "
    val randString = new StringBuilder
    val count = (Math.random * 30).toInt
    for (i <- 0 until count) {
      randString.append(symbols.charAt((Math.random * symbols.length).toInt))
    }
    randString.toString
  }

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    val mprint = new Consumer[String] {
      override def accept(t: String) = System.out.print(t + " ")
    }
    val comp = new Comparator[String] {
      override def compare(o1: String, o2: String) = o1.toString.compareToIgnoreCase(o2.toString)
    }
    var list = new ListArray[String]
    for(counter <- 0 to 30 ){
      var s: String=counter.toString();
      list.add(s)
    }
    list.print()
    System.out.println()

    for (counter <- 100 to 110 ) {
      var s: String=counter.toString();
      list.insert(s, 3)
    }
    list.print()
    System.out.println()

    list.sort((a,b)=>a.compare(b)>0);
    list.print()
    System.out.println()

    var s=list.get(3)
    System.out.println(s)
    System.out.println()

    for(counter <- 0 to 11 ){
      list.remove( 3)
    }
    list.print()
    System.out.println()

  }*/

  }
}
