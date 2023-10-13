import dataParsers.*
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.Node
import javafx.scene.chart.XYChart
import javafx.scene.control.{Alert, Label}
import javafx.scene.input.MouseEvent
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{BarChart, CategoryAxis, NumberAxis, StackedBarChart}
import scalafx.scene.chart.XYChart.Series
import scalafx.scene.control.*
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text}
import scalafx.stage.Stage
import scala.collection.mutable.*

class Pylväs(array: Array[DataPoint]) extends StatisticBoardPoints(array) {
  private var stg: Option[List[(String,(String,Color))]] = None
  private var series =  Buffer[XYChart.Series[String, Number]]()
  private val date = array.map(_.getData._2._2.get).distinct.toSeq
  private val xAxis = CategoryAxis(array.map(_.getData._1).distinct.toSeq)
  private val uprB = array.map(_.getData._4.split(" - ").head.toInt).max + (10 - array.map(_.getData._4.split(" - ").head.toInt).max % 10)
  val tikunit = if uprB + 10 >= 30000 then 100 else if uprB + 10 >= 15000 then 50 else 10
  private val yAxis = NumberAxis(axisLabel = "tapaukset", lowerBound = 0, upperBound = uprB, tickUnit = tikunit)
  private var barChart: Option[BarChart[String,Number]] = None

  /** this function changes chart's status to defined */
  def makePic(name: String, mittari : String): Unit = {
    val picture = new BarChart[String,Number](xAxis, yAxis) {
      title = "Stacked Bar Chart"
      categoryGap = 2
      val dataInstances = getData.get.sortBy(x => x.getData._1)
      //tämän verran tarvitaan myös seriesia
      val distincted_places = distinct(dataInstances.toList.map(x => x.getData._1))
      XYchartCreator
    }
    series.foreach(x => picture.getData.add(x))
    color_in(picture)
    barChart = Some(picture)

    }

  /** clicker is a finction to give more information about point when mouse is clicked */
  def clicker(): Unit =
    var info: Option[Stage] = None
    barChart.get.data.value.forEach(x=> x.getData.forEach(elem =>{
      elem.getNode.addEventHandler(MouseEvent.MOUSE_CLICKED,
        new EventHandler[MouseEvent]{
          override def handle(t: MouseEvent): Unit =
            val finder_1 = elem.getXValue
            val finder_2 = x.getName
            val xt = x
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
                add_comment(textInputButton,textfield,elem,xt)
                // event listener formed to change the color
                color_changer_of_piece(colorButton,elem,finder_1,xt)
                getComments(getCommentsButton,data_Instance)
                box.getChildren.addAll(colorButton,textfield,textInputButton,getCommentsButton)
                bp.center = box
                root = bp
              }
            }
            stage.show()
        })}))

  /** when mouse is targeting some point there is shown some basic additional data by this function */
  def mover: Text =
    val caption = new Text("")
    caption.font = Font.font(15.0)
    barChart.get.data.value.forEach(x => x.getData.forEach(elem =>{
      elem.getNode.addEventHandler(MouseEvent.MOUSE_MOVED,
        new EventHandler[MouseEvent]{
          override def handle(t: MouseEvent): Unit =
            caption.setTranslateX(t.getSceneX + 10.0)
            caption.setTranslateY(t.getSceneY - 10.0)
            caption.setText(elem.getYValue.toString + " - " + elem.getXValue)})}))
    barChart.get.data.value.forEach(x => x.getData.forEach(elem =>{
      elem.getNode.addEventHandler(MouseEvent.MOUSE_EXITED,
        new EventHandler[MouseEvent]{
          override def handle(t: MouseEvent): Unit =
            caption.setText("")})}))
    caption

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

  /** XYchart creator initialize XYseries with dataPoints */
  private def XYchartCreator =
    for i <- date do
      val datas = getData.get.filter(x =>{
        x.getData._2._2.get == i
      })
      val week = datas.head.getData._2._2.get
      val series1: XYChart.Series[String, Number] = new XYChart.Series[String, Number]
      series1.setName(i)
      datas.foreach(x =>
        {
          series1.getData.add(new XYChart.Data[String,Number](x.getData._1,x.getData._4.split(" - ").head.toInt))
        })
      series += series1

  /** Changes the color of chosen datapoint */
  private def color_changer_of_piece(button: Button, elem: javafx.scene.chart.XYChart.Data[String,Number], name: String, elem_2: javafx.scene.chart.XYChart.Series[String,Number]): Unit =
    button.onAction = (e: ActionEvent) =>
      val style = elem.getXValue
      val index = {
        val index_of_current_element = colorz.map(x => x._1).indexOf(elem.getNode.getStyle)
        if index_of_current_element + 1 >= colorz.length then
          0
        else
          index_of_current_element + 1
        }
      val tobeChangedInDataPoint = (name,elem_2.getName)
      //next lines will change information inside of datapoint according to the color change
      val color_of_dataPoint = getData.get.filter(x => x.getData._1 == tobeChangedInDataPoint._1 && x.getData._2._2.get == tobeChangedInDataPoint._2)
      if color_of_dataPoint.isEmpty then
        new Alert(Alert.AlertType.NONE,"No such a color exist").showAndWait()
      else
      //changes piece's color
      array.filter(_.getData._2._2.get == elem_2.getName).foreach(x => x.changeColor(colorz(index)))
      elem_2.getData.forEach(x => x.getNode.setStyle(colorz(index)._1))
      //to be chnaged
      val index_of_legend = getPic.lookupAll("Label.chart-legend-item").toBuffer.map(x => x.toString.split("'")(1)).indexOf(elem_2.getName)
      val legend_elem = getPic.lookupAll("Label.chart-legend-item").toBuffer(index_of_legend)
      val label = legend_elem.asInstanceOf[Label]
      val rectangle = new javafx.scene.shape.Rectangle(17.0,17.0)
      rectangle.setFill(colorz(index)._2)
      //chnages legend's color
      label.setGraphic(rectangle)

  /** add comment to datapoint */
  private def add_comment(button: Button, textField: TextField, elem: javafx.scene.chart.XYChart.Data[String,Number],elem_2: javafx.scene.chart.XYChart.Series[String,Number]) =
    button.onAction = (e: ActionEvent) =>
      val dataPoint_data = (elem.getXValue,elem_2.getName)
      val where_to_Save_comment = getData.get.filter(x => x.getData._1 == dataPoint_data._1 && x.getData._2._2.get == dataPoint_data._2)
      if textField.getText != "" && where_to_Save_comment.nonEmpty then
        where_to_Save_comment.head.addComment(textField.getText)
      else
        new Alert(Alert.AlertType.NONE,"No such a color exist").showAndWait()
      where_to_Save_comment.head.getComment.foreach(println)

  def getPic: scalafx.scene.chart.Chart = barChart.get

  override def changeAxisName(name: String) =
    barChart match
      case None => new Alert(Alert.AlertType.INFORMATION,"Please, create diagram first").showAndWait()
      case Some(value) => value.getYAxis.setLabel(name)

  override def changeName(name: String): Unit =
    barChart match
      case None => new Alert(Alert.AlertType.INFORMATION,"Please, create diagram first").showAndWait()
      case Some(value) => value.title = name

  override def getChartName = barChart.get.getTitle
  override def getAxisName = barChart.get.getYAxis.getLabel

  /** this function works in pair with fileReader to define all colors that were changed before */
  def color_in(chart: BarChart[String,Number]) =
    for i <- date do
      val datas = getData.get.filter(x =>{
        x.getData._2._2.get == i
      })
      chart.data.value.forEach(x => {
        x.getData.forEach(y => {
          val point_to_consider = datas.filter(z =>{
            z.getData._1 == y.getXValue && z.getData._2._2.get == x.getName})
          if point_to_consider.nonEmpty then
            point_to_consider.head.getColor match
              case None =>
              case Some(value) =>
                y.getNode.setStyle(value._1)
                val index_of_legend = chart.lookupAll("Label.chart-legend-item").toBuffer.map(x => x.toString.split("'")(1)).indexOf(x.getName)
                val legend_elem = chart.lookupAll("Label.chart-legend-item").toBuffer(index_of_legend)
                val label = legend_elem.asInstanceOf[Label]
                val rectangle = new javafx.scene.shape.Rectangle(17.0,17.0)
                rectangle.setFill(value._2)
                //chnages legend's color
                label.setGraphic(rectangle)
        })
      })


}
