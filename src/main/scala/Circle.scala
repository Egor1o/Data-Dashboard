import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.Side
import javafx.scene.control.{Alert, Label}
import javafx.scene.effect.Reflection
import javafx.scene.input.MouseEvent
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.PieChart
import scalafx.scene.effect.Glow
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import scalafx.scene.control.Button
import scalafx.scene.text.{Font, Text}
import scalafx.stage.Stage
import javafx.scene.Node
import javax.swing.border.Border
import scala.collection.mutable.Buffer
import scala.language.postfixOps
import scalafx.scene.control.*


class Circle(array: Array[DataPoint]) extends StatisticBoardPoints(array) {
  private var pieachart: Option[PieChart] = None
  //stg variable contains only the name of given place + color codes needed for PieChart and legend's elemnt. Used especially during the initialization
  private var stg: Option[List[(String,(String,Color))]] = None

  
  /** propogates pieChart according to the measure code is given*/
  override def makePic(name: String, mittari: String): Unit=
    val picture = new PieChart {
      title = name
      clockwise = true
      val dataInstances = getData.get.sortBy(x => x.getData._1)
      val distincted_places = distinct(dataInstances.toList.map(x => x.getData._1))
      data = ObservableBuffer.from(dataInstances.map({ x =>
        var cases = x.getData._4.split(" - ")
        var loka = 0.0
        if cases.length >= 1 then
          try{ loka = cases.head.toInt*1.0}
          catch {
            case _ => loka = 0.0
          }
        PieChart.Data(x.getData._1 +" - "+ x.getData._2._2.getOrElse("helou") + " - " +loka.toInt, loka)
      }))
      val realtions_color_place = distincted_places.zip(colorz)
      stg = Some(realtions_color_place)
      data.value.forEach(
        x =>{
          val color = realtions_color_place.filter(y => y._1 == x.getName.split(" - ").head).head._2._1
          x.getNode.setStyle(color)
        }
      )
    }

    for item <- picture.lookupAll("Label.chart-legend-item") do
      apu_legend_colorChange(item)
    color_in(picture)


    pieachart = Some(picture)

  /** when mouse is targeting some point there is shown some basic additional data by this function */
  def mover: Text =
    val caption = new Text("")
    caption.font = Font.font(15.0)
    pieachart.get.data.value.forEach(elem =>{
      elem.getNode.addEventHandler(MouseEvent.MOUSE_MOVED,
        new EventHandler[MouseEvent]{
          override def handle(t: MouseEvent): Unit =
            caption.setTranslateX(t.getSceneX + 10.0)
            caption.setTranslateY(t.getSceneY - 10.0)
            caption.setText(elem.getPieValue.toInt.toString + " - " + elem.getName.split(" - ").head)})})
    pieachart.get.data.value.forEach(elem =>{
      elem.getNode.addEventHandler(MouseEvent.MOUSE_EXITED,
        new EventHandler[MouseEvent]{
          override def handle(t: MouseEvent): Unit =
            caption.setText("")
            })})
    caption

  /** propogates additional info for chosen object */
  def clicker(): Unit =
    var info: Option[Stage] = None
    pieachart.get.data.value.forEach(elem =>{
      elem.getNode.addEventHandler(MouseEvent.MOUSE_CLICKED,
        new EventHandler[MouseEvent]{
          override def handle(t: MouseEvent): Unit =
            val finder_1 = elem.getName.split(" - ").head
            val finder_2 = elem.getName.split(" - ")(1)
            val data_Instance = getData.get.filter(x => x.getData._1 == finder_1 && x.getData._2._2.get == finder_2).head
            val box = VBox()
            connector(data_Instance).foreach(x => box.getChildren.addAll(x))
            val stage = new Stage{
              title = "additional info"
              scene =  new Scene(300,300){
                val bp = new BorderPane()
                val colorButton = new Button("color")
                var textfield = new TextField()
                val textInputButton = new Button("save text")
                val getCommentsButton = new Button("get comments")
                add_comment(textInputButton,textfield,elem)
                // event listener formed to change the color
                color_changer_of_piece(colorButton,elem)
                getComments(getCommentsButton,data_Instance)
                box.getChildren.addAll(colorButton,textfield,textInputButton,getCommentsButton)
                bp.center = box
                root = bp
              }
            }
            stage.show()
        })})
  
  /** connector is helper function for creating additional data window */
  private def connector(dp: DataPoint) =
    var text_data = Buffer[Text]()
    val stats = dp.getData._3.split(" - ").zip(dp.getData._4.split(" - "))
    text_data += new Text(dp.getData._1)
    dp.getData._2._1 match
      case None =>
      case x => text_data += new Text("date " + x.get)
    dp.getData._2._2 match
      case None =>
      case x => text_data += new Text("week: " + x.get)
    stats.foreach(y => {
      text_data += new Text(y._1 + ": " + y._2)})
    text_data.foreach(x => x.font = Font.font(12.0))
    text_data

  def get_Picture: Option[PieChart] = pieachart

  /** supposed to change the color of PieChart's peace */
  private def color_changer_of_piece(button: Button, elem: javafx.scene.chart.PieChart.Data): Unit =
    button.onAction = (e: ActionEvent) =>
      val style = elem.getName
      val index = {
        val index_of_current_element = colorz.map(x => x._1).indexOf(elem.getNode.getStyle)
        if index_of_current_element + 1 >= colorz.length then
          0
        else
          index_of_current_element + 1
        }
      val tobeChangedInDataPoint = (elem.getName.split(" - ").head,elem.getName.split(" - ")(1))
      //next lines will change information inside of datapoint according to the color change
      val color_of_dataPoint = getData.get.filter(x => x.getData._1 == tobeChangedInDataPoint._1 && x.getData._2._2.get == tobeChangedInDataPoint._2)
      if color_of_dataPoint.isEmpty then
        new Alert(Alert.AlertType.NONE,"No such a color exist").showAndWait()
      else
       color_of_dataPoint.head.changeColor(colorz(index))
      //changes piece's color
      elem.getNode.setStyle(colorz(index)._1)
      val index_of_legend = pieachart.get.lookupAll("Label.chart-legend-item").toBuffer.map(x => x.toString.split("'")(1)).indexOf(elem.getName)
      val legend_elem = pieachart.get.lookupAll("Label.chart-legend-item").toBuffer(index_of_legend)
      val label = legend_elem.asInstanceOf[Label]
      val rectangle = new javafx.scene.shape.Circle(4.0,4.0,4.0)
      rectangle.setFill(colorz(index)._2)
      //chnages legend's color
      label.setGraphic(rectangle)

  /** changes color of legend especially during the initialization */
  private def apu_legend_colorChange(node : Node) : Unit =
    val info = node.toString.split("'")(1).split(" - ")
      val tobeSetted = stg.get.filter(x => x._1 == info.head).head._2._2
      val label = node.asInstanceOf[Label]
      val rectangle = new javafx.scene.shape.Circle(4.0,4.0,4.0)
      rectangle.setFill(tobeSetted)
      label.setGraphic(rectangle)
      
  /** add comment to datapoint */
  private def add_comment(button: Button, textField: TextField, elem: javafx.scene.chart.PieChart.Data) =
    button.onAction = (e: ActionEvent) =>
      val dataPoint_data = (elem.getName.split(" - ").head,elem.getName.split(" - ")(1))
      val where_to_Save_comment = getData.get.filter(x => x.getData._1 == dataPoint_data._1 && x.getData._2._2.get == dataPoint_data._2)
      if textField.getText != "" && where_to_Save_comment.nonEmpty then
        where_to_Save_comment.head.addComment(textField.getText)
      else
        new Alert(Alert.AlertType.NONE,"No such a color exist").showAndWait()

  
  def getPic = pieachart.get

  override def changeAxisName(name: String): Unit = print("")
  override def changeName(name: String): Unit = 
    pieachart match
      case None => new Alert(Alert.AlertType.INFORMATION,"Please, create diagram first").showAndWait()
      case Some(value) => value.title = name
      
  override def getChartName = pieachart.get.getTitle
  override def getAxisName = ""

  /** this function works in pair with fileReader to define all colors that were changed before
   * it's not possible to define them in XYseries creation, because function does not
   * have node at that point and getNode function does not work for XYData*/
  def color_in(chart: PieChart) =
    chart.getData.forEach(x=> {
       val dataPoint_data = (x.getName.split(" - ").head,x.getName.split(" - ")(1))
       val parsattu = array.filter(y => y.getData._1 == dataPoint_data._1 && y.getData._2._2.get == dataPoint_data._2).head
       parsattu.getColor match
         case None =>
         case Some(value) =>
           x.getNode.setStyle(value._1)
           val index_of_legend = chart.lookupAll("Label.chart-legend-item").toBuffer.map(x => x.toString.split("'")(1)).indexOf(x.getName)
           val legend_elem = chart.lookupAll("Label.chart-legend-item").toBuffer(index_of_legend)
           val label = legend_elem.asInstanceOf[Label]
           val rectangle = new javafx.scene.shape.Circle(4.0,4.0,4.0)
           rectangle.setFill(value._2)
           //chnages legend's color
           label.setGraphic(rectangle)

    })

}
