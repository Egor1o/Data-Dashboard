import javafx.event.ActionEvent
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.VBox
import scalafx.scene.control.{Button, ComboBox, TextField}
import scalafx.stage.Stage
import scalafx.scene.*
import GUI.{Diagram, a, place, place_1}
import scalafx.scene.layout.HBox
import scalafx.scene.Node
import scalafx.scene.text.{Font, Text}
import scalafx.scene.control.Alert

import scala.collection.mutable.Buffer
class AnalyzeStage(diagram: Array[Diagram],datapoints: Array[DataPoint]) extends Stage{
  new Alert(Alert.AlertType.Information,"please notice that this data works only with diagram that you applied it with\nif you created newdiagram or updated older one, you will need\n" +
    "to make Analyze one more time").showAndWait()
  private var analyzeHimself: Option[Stage] = None
  private var places_added_by_button = Buffer[String]()
  title = "Analyze Stage settings"
  val bp = new BorderPane()
  scene = new Scene(600,600){
    var i = 0
    val contentVBox = new VBox()
    //first keys are checked and defiend
    val keys = {
      if diagram.map(_.placeIdentifier).contains("Kaikki Alueet") then Array("1","2","3","4","5")
      else if diagram.map(_.placeIdentifier).contains("shp") then Array("1","2")
      else if diagram.map(_.placeIdentifier).contains("kunta") then Array("1")
      else
        val alert = new Alert(Alert.AlertType.Information, "There is nothing to analyze")
        alert.showAndWait()
        Array[String]()
    }
    //creating mittari_combobx which will reagete to any changes
    val mittari_combobox = new ComboBox[String]()
    val mittariText = new Text("Choose measure")
    keys.foreach(x => mittari_combobox.getItems.addAll(x))
    mittari_combobox.items.apply().addAll(null)
    val mittariHBox = new HBox(mittariText,mittari_combobox)
    mittari_combobox.setTranslateX(mittariText.getTranslateX + 5)
    contentVBox.getChildren.addAll(mittariHBox)
    //creatingAnalyze box
    val analyzeCriteriaComboBox = new ComboBox[String](List("Minimal value", "Maximal Value", "Standart Deviation","Summary","Average"))
    val analyzeCriteriaText = new Text("Choose criteria")
    val analyzeCriteriaHBox = new HBox(analyzeCriteriaText,analyzeCriteriaComboBox)
    analyzeCriteriaComboBox.setTranslateX(analyzeCriteriaText.getTranslateX + 5)
    //now we define the places that contains choosen key
    var places = Buffer[Diagram]()
    val placesComboBox = new ComboBox[String]()
    val placesText = new Text("Choose place")
    val placeHBox = new HBox(placesText,placesComboBox)
    val intervalBox_1 = new ComboBox[Int]()
    val intervalBox_2 = new ComboBox[Int]()
    val intervalHBox = new HBox(intervalBox_1,intervalBox_2)
    val addPlaceButton = new Button("Add place/s")
    val addbutton_placebox = new VBox(placeHBox,addPlaceButton)
    val text = new Text("Added places")
    val place_tex_vbox = new VBox(text)
    val create_button = new Button("Create statistc stage")
    placesText.setTranslateX(placesText.getTranslateX - 5)
    placesComboBox.setTranslateX(placesText.getTranslateX + 5)
    bp.center = contentVBox
    //mittaribox Action handling
    mittari_combobox.onAction = (e:ActionEvent) =>
      places_added_by_button.clear()
      contentVBox.getChildren.clear()
      place_tex_vbox.children.clear()
      addbutton_placebox.children.remove(place_tex_vbox)
      intervalBox_1.items.value.clear()
      intervalBox_2.items.value.clear()
      intervalHBox.children.clear()
      intervalHBox.children.addAll(new Text("Choose interval"), intervalBox_1,intervalBox_2)
      if contentVBox.children.contains(placeHBox) then contentVBox.children.remove(placeHBox)
      analyzeCriteriaComboBox.items.value.clear()
      analyzeCriteriaComboBox.items.value.addAll("Minimal value", "Maximal Value", "Standart Deviation", "Summary","Average")
      contentVBox.getChildren.addAll(mittariHBox,analyzeCriteriaHBox)
      bp.children.remove(bp.getRight)



    analyzeCriteriaComboBox.onAction = (e:ActionEvent) =>
      places = Buffer[Diagram]()
      placesComboBox.items.apply().clear()
      val selected_mittari = mittari_combobox.selectionModel.apply().getSelectedItem
      if Array("3","4","5").contains(selected_mittari) then
        places = diagram.filter(_.placeIdentifier == "Kaikki Alueet").toBuffer
      else if Array("2").contains(selected_mittari) then
        places = diagram.filter(x => x.placeIdentifier == "Kaikki Alueet" || x.placeIdentifier == "shp").distinct.toBuffer
      else
        places = diagram.toBuffer

      if places.nonEmpty then
        places.foreach(x => placesComboBox.items.apply().addAll(x.place))
        if !bp.children.contains(addbutton_placebox) then
          addbutton_placebox.children.remove(place_tex_vbox)
          addbutton_placebox.children.addAll(place_tex_vbox)
          bp.right = addbutton_placebox
      else
        new Alert(Alert.AlertType.Error,"There is no places with this criteria")

    addPlaceButton.onAction = (e:ActionEvent) =>
      if placesComboBox.selectionModel.apply().getSelectedItem == null then
        new Alert(Alert.AlertType.Error, "Nothing has been choosen").showAndWait()
      else
        if !places_added_by_button.contains(placesComboBox.selectionModel.apply().getSelectedItem) then
          if places_added_by_button.isEmpty then
            place_tex_vbox.children.addAll(new Text("Added places: "))
          places_added_by_button += placesComboBox.selectionModel.apply().getSelectedItem
          place_tex_vbox.children.addAll(new Text(placesComboBox.selectionModel.apply().getSelectedItem))
          //intervalien toiminta
          val paikat_start_end = diagram.filter(x => places_added_by_button.contains(x.place)).map(y => (y.place,y.start,y.end))
          intervalBox_1.items.value.clear()
          intervalBox_2.items.value.clear()
          val lowest = paikat_start_end.map(_._2).max
          val greatest = paikat_start_end.map(_._3).min
          if lowest > greatest then
            new Alert(Alert.AlertType.Error, "there is no common interval in choosen places").showAndWait()
          else
            for i <- lowest to greatest do
              intervalBox_1.items.value.addAll(i)
              intervalBox_2.items.value.addAll(i)
              addbutton_placebox.children.remove(intervalHBox)
              addbutton_placebox.children.remove(create_button)
            addbutton_placebox.children.addAll(intervalHBox,create_button)
        else
          new Alert(Alert.AlertType.Error, "This is already in list").showAndWait()

      create_button.onAction = (e: ActionEvent) =>
        if !intervalBox_1.selectionModel.apply().getSelectedItem.isInstanceOf[Int] && !intervalBox_1.selectionModel.apply().getSelectedItem.isInstanceOf[Int] && intervalBox_1.selectionModel.apply().getSelectedItem > intervalBox_2.selectionModel.apply().getSelectedItem then
          new Alert(Alert.AlertType.Error,"You either has not choosen interval, or your start interval is bigger than end interval").showAndWait()
        else
          val start_in = intervalBox_1.selectionModel.apply().getSelectedItem
          val end_in = intervalBox_2.selectionModel.apply().getSelectedItem
          val point: Array[DataPoint] = datapoints.filter(x =>
            places_added_by_button.map(x => if x == "Kaikki Alueet" then x.toLowerCase else x).contains(x.getData._1) && x.getData._2._2.get.toInt >= start_in && x.getData._2._2.get.toInt <= end_in)
          analyzeCriteriaComboBox.selectionModel.value.getSelectedItem match
            case null => new Alert(Alert.AlertType.Error,"Choose the measure")
            case "Maximal Value" =>
              val data = maxValue((start_in,end_in),point,mittari_combobox.selectionModel.apply().getSelectedItem)
              new Stage{
                title = "Analyze"
                scene = new Scene(400,400){
                  val borderPa = new BorderPane()
                  val place = new Text("Place of point: " + data._1)
                  place.font = Font.font(15.0)
                  val weekkk = new Text("Week of the point: " + data._2)
                  weekkk.font = Font.font(15.0)
                  val value = new Text(data._3.toString)
                  value.font = Font.font(15.0)
                  val interval_of = new Text("Week interval is: " +start_in.toString + " - " +end_in.toString)
                  interval_of.font = Font.font(15.0)
                  place.setTranslateY(place.getTranslateY + 3)
                  weekkk.setTranslateY(place.getTranslateY + 4)
                  value.setTranslateY(weekkk.getTranslateY + 4)
                  interval_of.setTranslateY(value.getTranslateY + 4)
                  borderPa.center = new VBox(place,weekkk,value,interval_of)
                  root = borderPa
                }
              }.show()
            case "Summary" =>
              val data = allValuesFromInterval((start_in,end_in),point,mittari_combobox.selectionModel.apply().getSelectedItem)
              new Stage{
                title = "Analyze"
                scene = new Scene(400,400){
                  val borderPa = new BorderPane()
                  val places_by_separator = places_added_by_button.mkString("\n")
                  val place = new Text("Place of point: " + places_by_separator)
                  place.font = Font.font(15.0)
                  val value = new Text("Sum of the values by measure - " + mittariDefiner(mittari_combobox.selectionModel.value.getSelectedItem) + " is: " + data.toString)
                  value.font = Font.font(15.0)
                  val interval_of = new Text("Week interval is: " +start_in.toString + " - " +end_in.toString)
                  interval_of.font = Font.font(15.0)
                  place.setTranslateY(place.getTranslateY + 3)
                  value.setTranslateY(place.getTranslateY + 4)
                  interval_of.setTranslateY(value.getTranslateY + 4)
                  borderPa.center = new VBox(place,value,interval_of)
                  root = borderPa
                }
              }.show()
            case "Minimal value" =>
              val data = minValue((start_in,end_in),point,mittari_combobox.selectionModel.apply().getSelectedItem)
              new Stage{
                title = "Analyze"
                scene = new Scene(400,400){
                  val borderPa = new BorderPane()
                  val place = new Text("Place of point: " + data._1)
                  place.font = Font.font(15.0)
                  val weekkk = new Text("Week of the point: " + data._2)
                  weekkk.font = Font.font(15.0)
                  val value = new Text(data._3.toString)
                  value.font = Font.font(15.0)
                  val interval_of = new Text("Week interval is: " +start_in.toString + " - " +end_in.toString)
                  interval_of.font = Font.font(15.0)
                  place.setTranslateY(place.getTranslateY + 3)
                  weekkk.setTranslateY(place.getTranslateY + 4)
                  value.setTranslateY(weekkk.getTranslateY + 4)
                  interval_of.setTranslateY(value.getTranslateY + 4)
                  borderPa.center = new VBox(place,weekkk,value,interval_of)
                  root = borderPa
                }
              }.show()
            case "Standart Deviation" =>
              val data = StandartDeviation((start_in,end_in),point,mittari_combobox.selectionModel.apply().getSelectedItem)
              new Stage{
                title = "Analyze"
                scene = new Scene(400,400){
                  val borderPa = new BorderPane()
                  val places_by_separator = places_added_by_button.mkString("\n")
                  val place = new Text("Place of point: " + places_by_separator)
                  place.font = Font.font(15.0)
                  val value = new Text("Standart Deviation is: "  + data.toString)
                  value.font = Font.font(15.0)
                  val interval_of = new Text("Week interval is: " +start_in.toString + " - " +end_in.toString)
                  interval_of.font = Font.font(15.0)
                  place.setTranslateY(place.getTranslateY + 3)
                  value.setTranslateY(place.getTranslateY + 4)
                  interval_of.setTranslateY(value.getTranslateY + 4)
                  borderPa.center = new VBox(place,value,interval_of)
                  root = borderPa
                }
              }.show()
            case "Average" =>
              val data = keskiArvo((start_in,end_in),point,mittari_combobox.selectionModel.apply().getSelectedItem)
              new Stage{
                title = "Analyze"
                scene = new Scene(400,400){
                  val borderPa = new BorderPane()
                  val places_by_separator = places_added_by_button.mkString("\n")
                  val place = new Text("Place of point: " + places_by_separator)
                  place.font = Font.font(15.0)
                  val value = new Text("Avergae is: "  + data.toString)
                  value.font = Font.font(15.0)
                  val interval_of = new Text("Week interval is: " +start_in.toString + " - " +end_in.toString)
                  interval_of.font = Font.font(15.0)
                  place.setTranslateY(place.getTranslateY + 3)
                  value.setTranslateY(place.getTranslateY + 4)
                  interval_of.setTranslateY(value.getTranslateY + 4)
                  borderPa.center = new VBox(place,value,interval_of)
                  root = borderPa
                }
              }.show()
              




    root = bp
  }

  private def maxValue(interval: (Int,Int),data: Array[DataPoint],key: String) =
    val wordkey = mittariDefiner(key)
    val mappedpoints = data.map(x => (x.getData._1,x.getData._2._2.get,x.getData._3.split(" - ").indexOf(wordkey),x.getData._4.split(" - ")))
    val sortedpoints = mappedpoints.map(x => (x._1,x._2,x._4(x._3).toInt)).maxBy(x => x._3)
    sortedpoints

  private def minValue(interval: (Int,Int),data: Array[DataPoint],key: String) =
    val wordkey = mittariDefiner(key)
    val mappedpoints = data.map(x => (x.getData._1,x.getData._2._2.get,x.getData._3.split(" - ").indexOf(wordkey),x.getData._4.split(" - ")))
    val sortedpoints = mappedpoints.map(x => (x._1,x._2,x._4(x._3).toInt)).minBy(x => x._3)
    sortedpoints

  private def allValuesFromInterval(interval: (Int,Int),data: Array[DataPoint],key: String) =
    val wordkey = mittariDefiner(key)
    val mappedpoints = data.map(x => (x.getData._1,x.getData._2._2.get,x.getData._3.split(" - ").indexOf(wordkey),x.getData._4.split(" - ")))
    val sortedpoints = mappedpoints.map(x => (x._1,x._2,x._4(x._3).toInt)).map(_._3).sum
    sortedpoints

  private def keskiArvo(interval: (Int,Int),data: Array[DataPoint],key: String) =
    val wordkey = mittariDefiner(key)
    val mappedpoints = data.map(x => (x.getData._1,x.getData._2._2.get,x.getData._3.split(" - ").indexOf(wordkey),x.getData._4.split(" - ")))
    val arvot = mappedpoints.map(x => (x._1,x._2,x._4(x._3).toInt)).map(_._3)
    val keskiarvo = (arvot.sum*1.0)/(arvot.length*1.0)
    keskiarvo

  private def StandartDeviation(interval: (Int,Int),data: Array[DataPoint],key: String) =
    val wordkey = mittariDefiner(key)
    val mappedpoints = data.map(x => (x.getData._1,x.getData._2._2.get,x.getData._3.split(" - ").indexOf(wordkey),x.getData._4.split(" - ")))
    val arvot = mappedpoints.map(x => (x._1,x._2,x._4(x._3).toInt)).map(_._3)
    try{
      val keskiarvo = (arvot.sum*1.0)/(arvot.length*1.0)
      val summarry = arvot.map(x => scala.math.pow((x * 1.0)-keskiarvo,2.0)).sum
      val deviation = scala.math.sqrt(summarry/((arvot.length -1)*1.0))
      deviation}
    catch{
      case _ => new Alert(Alert.AlertType.Error,"Standart deviation can not be calculated with these values").showAndWait()
    }


  private def mittariDefiner(code: String) =
    code match
        case "3" => "deathes"
        case "1" => "corona cases"
        case "2" => "tests"
        case "5" => "death because of corona"
        case "4" => "other_deathes"
}
