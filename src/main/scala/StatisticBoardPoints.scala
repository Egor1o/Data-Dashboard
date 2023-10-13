
import dataParsers.*
import javafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text}
import scalafx.stage.Stage

import scala.collection.mutable.Buffer
trait StatisticBoardPoints(points: Array[DataPoint]):


  private var comments: Buffer[String] = Buffer()
  private var data: Option[Buffer[DataPoint]] = Some(points.toBuffer)
  private var colors = List(
    ("-fx-background-color: #ffa07a",Color.web("#ffa07a")),
    ("-fx-background-color: #fffacd",Color.web("#fffacd")),
    ("-fx-background-color: #d8bfd855",Color.web("#d8bfd855")),
    ("-fx-background-color: #2f4f4f",Color.web("#2f4f4f")),
    ("-fx-background-color: #228b22",Color.web("#228b22")),
    ("-fx-background-color: #ffd700",Color.web("#ffd700")),
    ("-fx-background-color: #ff69b4",Color.web("#ff69b4")),
    ("-fx-background-color: #48d1cc",Color.web("#48d1cc")),
    ("-fx-background-color: #ffa07a",Color.web("#ffa07a")),
    ("-fx-background-color: #b22222",Color.web("#b22222")))



  def getAxisName: String
  def getChartName: String
  def changeName(s: String): Unit
  def makePic(name: String, mittari: String): Unit
  def changeAxisName(name: String): Unit
  def dataInit(data: Array[DataPoint]) =
    this.data match
      case None => this.data = Some(data.toBuffer)
      case _ => this.data = Some(this.data.get ++ data.toBuffer)

  def getData: Option[Buffer[DataPoint]] = data

  def colorz = colors

  /** gettting rid of duplicates in list */
  def distinct[String](list:List[String]):List[String] =
  list.foldLeft(List[String]()) {
    case (acc, item) if acc.contains(item) => acc
    case (acc, item) => item::acc
  }
  
  /** returns comments at new stage */
  def getComments(button: Button, point: DataPoint) =
    button.onAction = (e: ActionEvent) =>
      val comment_stage = new Stage{
        title = "comment stage"
        scene = new Scene(250,250){
          val bp_shka = new BorderPane()
          val comments = point.getComment
          var counter = 1
          val toBeRepresented =  comments.map(com =>{
            var commet = com
            val be_DivideBy = com.length/25
            var will_be_in_comment = ""
            for i <- 0 to be_DivideBy + 1 do
              will_be_in_comment += commet.take(25) + "\n"
              commet = commet.drop(25)
            counter += 1
            new Text((counter-1).toString + ". " + will_be_in_comment)})
          toBeRepresented.foreach(x => x.font = Font.font(15.0))
          val vbox = new VBox()
          toBeRepresented.foreach(x => vbox.getChildren.addAll(x))
          vbox.setTranslateX(vbox.getTranslateX + 4)
          bp_shka.center = vbox
          root = bp_shka
        }
      }
      comment_stage.show()
  
          

