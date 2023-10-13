import GUI.*
import dataParsers.*
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import javafx.application.Application
import javafx.collections.{FXCollections, ObservableArray, ObservableList}
import javafx.event.EventHandler
import javafx.scene.control.cell.PropertyValueFactory
import javafx.scene.control.{MenuBar, MenuItem}
import javafx.scene.input.MouseEvent
import javafx.scene.layout.BorderPane
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.*
import scalafx.scene.chart.*
import scalafx.scene.control.*
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.*
import scalafx.scene.paint.*
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Arc
import scalafx.scene.text.{Font, Text}
import scalafx.stage.Stage
import java.awt.font.ImageGraphicAttribute
import java.io.*
import java.util.Scanner
import scala.collection.mutable.Buffer
import scala.util.{Failure, Success, Try}

class AfterWordMaintainer extends Stage{
  /** measures codes */
  private val death_measure_code = "492118"
  private val case_measure_code = "444833"
  private val tested_measure_code = "445356"
  private val defined_death_because_of_corona = "816930"
  private val other_death = "816957"
  title = "new diagram"
  scene = new Scene(900, 550){
    val borderPane = new scalafx.scene.layout.BorderPane()
    val dataBox = new VBox()
    val interval = (a.getWeek_coodes.head._1,a.getWeek_coodes.last._1)
    val cmobox_1 = new ComboBox[Int]()
    val cmobox_2 = new ComboBox[Int]()
    val cmobox_3 = new ComboBox[String]()
    cmobox_3.items.value.addAll("Bar Chart", "Pie Chart", "Line Chart")
    val removeButton = new Button("Remove")
    val intervalbox = new HBox()
    a.getWeek_coodes.take(a.getWeek_coodes.length-1).foreach(x => cmobox_1.items.value.addAll(x._1))
    a.getWeek_coodes.take(a.getWeek_coodes.length-1).foreach(x => cmobox_2.items.value.addAll(x._1))
    intervalbox.children.addAll(cmobox_1,cmobox_2)
    intervalbox.setTranslateY(intervalbox.getTranslateY + 5)
    cmobox_2.setTranslateX(cmobox_2.getTranslateX + 10)
    val layout = new Label(s"Choose week interval\nfrom ${interval._1} to ${interval._2}\nwhere 0 is (2020, week 0)\n208 is (2023, week 52")
    val ShpSelector = new ComboBox[String]()
    val kuntaCreator = new ComboBox[String]()
    a.getSHP_names.filter(_._2 != "Kaikki Alueet").foreach(x => ShpSelector.items.value.addAll(x._2))
    ShpSelector.items.value.addAll(null)
    val measureSelector = new ComboBox[String]()
    val mittariBox = new VBox(new Text("Choose measure"),measureSelector)
    val kaikki_alueet_Shp_selector = new ComboBox[String]()
    kaikki_alueet_Shp_selector.items.apply().addAll("Kaikki Alueet", "Regions")
    val CenterHbox = new HBox()
    measureSelector.items.apply().addAll("1","2","3","4","5")
    val place_choose = new VBox()
    place_choose.setTranslateY(place_choose.getTranslateY + 3)
    CenterHbox.setTranslateX(CenterHbox.getTranslateX + 40)
    place_choose.getChildren.addAll(new Text("Choose the place"),kaikki_alueet_Shp_selector)
    mittariBox.setTranslateX(mittariBox.getTranslateX + 40)
    mittariBox.setTranslateY(mittariBox.getTranslateY + 3)
    CenterHbox.getChildren.addAll(place_choose,mittariBox)


    kaikki_alueet_Shp_selector.onAction = (e: ActionEvent) =>
      if kaikki_alueet_Shp_selector.selectionModel.apply().getSelectedItem == "Regions" then
        place_choose.children.addAll(ShpSelector)
      else
        place_choose.getChildren.remove(ShpSelector)
        ShpSelector.items.value.clear()
        a.getSHP_names.filter(_._2 != "Kaikki Alueet").foreach(x => ShpSelector.items.value.addAll(x._2))
        kuntaCreator.getSelectionModel.selectPrevious()
        place_choose.getChildren.remove(kuntaCreator)
        measureSelector.items.value.clear()
        measureSelector.items.apply().addAll("1","2","3","4","5")

    ShpSelector.onAction = (e: ActionEvent)  =>
      if ShpSelector.selectionModel.apply().getSelectedItem != null then
        if place_choose.children.length < 4 then
          place_choose.children.addAll(kuntaCreator)
        val name = ShpSelector.selectionModel.apply().getSelectedItem
        kuntaCreator.items.value.clear()
        a.getShp_kunnat.filter(x => x._1 == name).head._2.foreach(y => kuntaCreator.items.value.add(y._2))
        measureSelector.items.value.clear()
        measureSelector.items.apply().addAll("1","2")
        if !ShpSelector.items.value.contains(null) then ShpSelector.items.value.addAll(null)
      else
        kuntaCreator.items.value.clear()
        measureSelector.items.value.clear()
        measureSelector.items.apply().addAll("1","2","3","4","5")
        place_choose.getChildren.remove(kuntaCreator)

    kuntaCreator.onAction = (e: ActionEvent) =>
      if kuntaCreator.selectionModel.apply().getSelectedItem != null then
        measureSelector.items.value.clear()
        measureSelector.items.apply().addAll("1")

    val addButton = new Button("Add")
    val SaveButton = new Button("Create Diagram")
    val add_remove_savebuttonBox = new HBox()
    SaveButton.setTranslateX(SaveButton.getTranslateX + 3)
    addButton.setTranslateY(addButton.getTranslateY + 70)
    SaveButton.setTranslateY(addButton.getTranslateY)
    removeButton.setTranslateX(SaveButton.getTranslateX + 3)
    removeButton.setTranslateY(SaveButton.getTranslateY)
    add_remove_savebuttonBox.children.addAll(addButton)

    val table = new TableColumn[String, String]()

    val typeText = new Text("Choose type of diagram")
    typeText.setTranslateY(typeText.getTranslateY + 3)
    val addedText = new Text("Added diagrams")
    add_remove_savebuttonBox.children.clear()
    add_remove_savebuttonBox.children.addAll(addButton,SaveButton,removeButton)
    def refactor =
      cmobox_1.items.value.clear()
      cmobox_2.items.value.clear()
      cmobox_3.items.value.clear()
      cmobox_3.items.value.addAll("Bar Chart", "Pie Chart", "Line Chart")
      a.getWeek_coodes.foreach(x => cmobox_1.items.value.addAll(x._1))
      a.getWeek_coodes.foreach(x => cmobox_2.items.value.addAll(x._1))
      place_choose.getChildren.remove(ShpSelector)
      ShpSelector.items.value.clear()
      a.getSHP_names.filter(_._2 != "Kaikki Alueet").foreach(x => ShpSelector.items.value.addAll(x._2))
      kuntaCreator.getSelectionModel.selectPrevious()
      place_choose.getChildren.remove(kuntaCreator)
      measureSelector.items.value.clear()
      measureSelector.items.apply().addAll("1","2","3","4","5")
      kaikki_alueet_Shp_selector.items.value.clear()
      kaikki_alueet_Shp_selector.items.apply().addAll("Kaikki Alueet", "Regions")

    addButton.onAction = (e: ActionEvent) =>
      val first_d = cmobox_1.selectionModel.apply().getSelectedItem
      val b = cmobox_2.selectionModel.apply().getSelectedItem
      val mittari_selector = measureSelector.selectionModel.apply().getSelectedItem
      val kun = kuntaCreator.selectionModel.apply().getSelectedItem
      val shp = ShpSelector.selectionModel.apply().getSelectedItem
      val kaikki = kaikki_alueet_Shp_selector.selectionModel.apply().getSelectedItem
      val type_of_diag = cmobox_3.selectionModel.apply().getSelectedItem
      add_remove_savebuttonBox.children.clear()
      add_remove_savebuttonBox.children.addAll(addButton,SaveButton,removeButton)
      if GUI.getDiagrams.nonEmpty then
        val in_start = GUI.getDiagrams.map(x => x.start).min
        val in_end = GUI.getDiagrams.map(x => x.end).max
        try{
          require(first_d.isInstanceOf[Int] && b.isInstanceOf[Int])
          require(first_d <= b && first_d <= in_end && b >= in_start)
          require(GUI.getDiagrams.map(x => x.measure).contains(mittari_selector))
          require(GUI.getDiagrams.map(x => x.D_type).contains(type_of_diag))

          if kun != null then
            if !GUI.getDiagrams.map(_.place).contains(kun) then
              val dg = new Diagram(kun ,"kunta",type_of_diag, first_d,b,mittari_selector,"")
              GUI.setDiagrams(GUI.getDiagrams ++ Buffer(dg))
              GUI.getTabulaaaar.get.getItems.addAll(dg)
            else
              new Alert(Alert.AlertType.Error,"This place is already in list").showAndWait()
          else if shp != null then
            if !GUI.getDiagrams.map(_.place).contains(shp) then
              val dg = new Diagram(shp ,"shp",type_of_diag, first_d, b, mittari_selector,"")
              GUI.setDiagrams(GUI.getDiagrams ++ Buffer(dg))
              GUI.getTabulaaaar.get.getItems.addAll(dg)
            else
              new Alert(Alert.AlertType.Error,"This place is already in list").showAndWait()
          else if kaikki != null && (kaikki == "Kaikki Alueet" || kaikki == "Regions") then
            if !GUI.getDiagrams.map(_.place).contains("Kaikki Alueet") then
              val dg = new Diagram("Kaikki Alueet","Kaikki Alueet" ,type_of_diag, first_d, b, mittari_selector, "")
              GUI.setDiagrams(GUI.getDiagrams ++ Buffer(dg))
              GUI.getTabulaaaar.get.getItems.addAll(dg)
            else
              new Alert(Alert.AlertType.Error,"This place is already in list").showAndWait()
          else
            require(0 > 10)
        }
        catch {
          case e: IllegalArgumentException =>
            println("require failed")
          case _ =>
            println("other exeption")
        }

      else
        try{
          require(first_d.isInstanceOf[Int] && b.isInstanceOf[Int])
          require(first_d <= b && first_d >= 0 && first_d <= a.getWeek_coodes.last._1 && b >= 0 && b <= a.getWeek_coodes.last._1 - 1)
          require(shp != null || kun != null || kaikki != null)
          require(mittari_selector != null && type_of_diag != null)

          if kun != null then
            val dg = new Diagram(kun ,"kunta",type_of_diag, first_d,b,mittari_selector,"")
            GUI.setDiagrams(GUI.getDiagrams ++ Buffer(dg))
            GUI.getTabulaaaar.get.getItems.addAll(dg)
          else if shp != null then
            val dg =  new Diagram(shp ,"shp",type_of_diag, first_d, b, mittari_selector,"")
            GUI.setDiagrams(GUI.getDiagrams ++ Buffer(dg))
            GUI.getTabulaaaar.get.getItems.addAll(dg)
          else if kaikki != null && kaikki == "Kaikki Alueet" || kaikki == "Regions" then
            val dg = new Diagram("Kaikki Alueet","Kaikki Alueet" ,type_of_diag, first_d, b, mittari_selector, "")
            GUI.setDiagrams(GUI.getDiagrams ++ Buffer(dg))
            GUI.getTabulaaaar.get.getItems.addAll(dg)
          add_remove_savebuttonBox.children.addAll(SaveButton,removeButton)
        }
        catch {
            case e: IllegalArgumentException => println("requirment failed")
            case _ => println("other exception")
          }

      refactor

    SaveButton.onAction = (e: ActionEvent) =>
      if GUI.getDiagrams.nonEmpty then
        var combined_Data: Buffer[DataPoint] = Buffer[DataPoint]()
        GUI.getDiagrams.foreach(x => {
          val mit = mittariDefiner(x.measure)
          val mit_2 = if mit == case_measure_code then tested_measure_code else case_measure_code
          x.placeIdentifier match
            case "kunta" =>
              val keys = a.getShp_kunnat.map(y => y._2)
              val index = keys.flatten.map(y => y._2).indexOf(x.place)
              val kuntacode = keys.flatten.apply(index)._1
              val toBeStored = a.kunta_Creator((x.start,x.end),Array(kuntacode))
              combined_Data = combined_Data ++ toBeStored.toBuffer
            case "shp" =>
              val tobeStored = a.SHP_creator_week((x.start,x.end),Array(a.getTriplat.filter(y => y._3 == x.place).head._2),Array(mit,mit_2))
              combined_Data = combined_Data ++ tobeStored.toBuffer
            case "Kaikki Alueet" =>
              val mit_3 = Array(mit) ++ Array(case_measure_code,tested_measure_code,death_measure_code,other_death,defined_death_because_of_corona).filter(_ != mit)
              val tobeStored = a.kaikki_alueet_viikko_mittari((x.start,x.end), mit_3)
              combined_Data = combined_Data ++ tobeStored.toBuffer
            case _ => println("Kaikki Alueet")
        })
        GUI.getDiagrams.head.D_type match
          case "Bar Chart" =>
            val pyl = new Pylväs(combined_Data.toArray)
            pyl.makePic("Bar Diagram","Bar Diagram")
            GUI.getRootPane.center = pyl.getPic
            GUI.getRootPane.getChildren.addAll(pyl.mover)
            pyl.clicker()
            GUI.setDiagramHimself(Some(pyl))
          case  "Pie Chart"=>
            val cir = new Circle(combined_Data.toArray)
            cir.makePic("Pie Chart","Pie Chart")
            GUI.getRootPane.center = cir.getPic
            GUI.getRootPane.getChildren.addAll(cir.mover)
            cir.clicker()
            GUI.setDiagramHimself(Some(cir))
          case  "Line Chart"=>
            val viv = new ViivaKaavio(combined_Data.toArray)
            viv.makePic("Line Diagram","Line Diagram")
            GUI.getRootPane.center = viv.getPic
            GUI.getRootPane.getChildren.addAll(viv.mover)
            viv.clicker()
            GUI.setDiagramHimself(Some(viv))
      else
        new Alert(Alert.AlertType.Error,"There is no items in list").showAndWait()

    removeButton.onAction = (e: ActionEvent) =>
      if GUI.getTabulaaaar.get.selectionModel.apply().getSelectedItem != null then
        val name_place = GUI.getTabulaaaar.get.selectionModel.apply().getSelectedItem.place
        var dgrms = GUI.getDiagrams
        dgrms.remove(dgrms.map(x => x.place).indexOf(name_place))
        GUI.setDiagrams(dgrms)
        GUI.getTabulaaaar.get.getItems.removeAll(GUI.getTabulaaaar.get.selectionModel.apply().getSelectedItem)
      else
        new Alert(Alert.AlertType.Warning,"Please, choose the place to remove").showAndWait()

    GUI.getTabulaaaar.get.setTranslateY(addButton.getTranslateY + 10)
    val typeBox = new VBox()
    typeBox.children.addAll(typeText,cmobox_3)
    typeBox.setTranslateX(measureSelector.getTranslateX + 60)
    CenterHbox.getChildren.addAll(typeBox)
    cmobox_3.setTranslateY(typeText.getTranslateY + 1)
    dataBox.getChildren.addAll(layout,intervalbox,add_remove_savebuttonBox,GUI.getTabulaaaar.get)
    dataBox.setTranslateY(dataBox.getTranslateY + 3)
    dataBox.setTranslateX(dataBox.getTranslateX + 3)

    borderPane.left = dataBox
    borderPane.center = CenterHbox
    root = borderPane
      }}

