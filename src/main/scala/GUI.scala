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
object GUI extends JFXApp3{

  case class Diagram(place: String, placeIdentifier: String, D_type: String, start: Int, end: Int, measure: String, additional_measures: String):
    def getPlace  =
      place
    def getD_type =
      D_type
    def getStart =
      start
    def getEnd =
      end
    def getMeasure =
      measure
    override def toString =
      place + " " + placeIdentifier + " "+ D_type + " " +start + " " +end + " " +measure + " " +additional_measures

  /** default adders to link */
  private val place = "row=hcdmunicipality2020-"
  private val place_1 = "hcdmunicipality2020"
  private val dataweek = "column=dateweek20200101-"
  private val dataweek_1 = "dateweek20200101"

  /** common codes for default links */
  private val all_regions = "445222"
  private val all_times = "509030"

  /** measures codes */
  private val death_measure_code = "492118"
  private val case_measure_code = "444833"
  private val tested_measure_code = "445356"
  private val defined_death_because_of_corona = "816930"
  private val other_death = "816957"

  private var diagramHimself: Option[StatisticBoardPoints] = None
  private var diagrams: Buffer[Diagram] = Buffer[Diagram]()
  private val rootPane = new BorderPane()
  private var tabulaaaar:Option[TableView[Diagram]] = None
  private var updateStage: Option[Stage] = None

  //DataReader should be actually an obect, but unfotrtunally it became clear for developer
  //not that long ago. So the only one instance is staying here to be helpful also in other
  //classes
  val a = new DataReader()

   override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Data Dashboard"
      scene = new Scene(1200, 1000){
        val menubar = new MenuBar
        /** menut */
        val fileMenu = new Menu("file")
        val settingsMenu = new Menu("settings")
        val updateMenu = new Menu("update")
        val compareMenu = new Menu("compare")
        val compareItem = new MenuItem("Compare")
        compareMenu.getItems.addAll(compareItem)
        val analyzeMenu = new Menu("analyze")
        menubar.menus = List(fileMenu, settingsMenu, updateMenu, compareMenu, analyzeMenu)
        val newItem = new MenuItem("New")
        val fileItem = new MenuItem("Open from file")
        val saveDiagram = new MenuItem("Save")
        fileMenu.items = List(newItem,fileItem,saveDiagram)
        rootPane.top = menubar
        val changeAxisName = new MenuItem("Change Axis Name")
        val changeChartName = new MenuItem("Change Chart's name")
        settingsMenu.items.addAll(changeAxisName,changeChartName)
        val Analyze = new MenuItem("Analyze")
        analyzeMenu.items.addAll(Analyze)
        val UpdateDiagram = new MenuItem("Update")
        updateMenu.items.addAll(UpdateDiagram)

        changeAxisName.onAction = (e: ActionEvent) =>
          diagramHimself match
            case None => new Alert(Alert.AlertType.Error,"There is no diagram yet").showAndWait()
            case Some(value) =>
              val a = new ChangeAxisNameStage(value, "axis")
              value match
                case p: Pylväs =>
                  a.show()
                case v: ViivaKaavio =>
                  a.show()
                case y: Circle =>

        changeChartName.onAction = (e: ActionEvent) =>
          diagramHimself match
            case None => new Alert(Alert.AlertType.Error,"There is no diagram yet").showAndWait()
            case Some(value) =>
              val a = new ChangeAxisNameStage(value, "name")
              value match
                case p: Pylväs =>
                  a.show()
                case v: ViivaKaavio =>
                  a.show()
                case y: Circle =>
                  a.show()


        newItem.onAction = (e:ActionEvent) =>
          diagrams = Buffer[Diagram]()
          diagramHimself = None
          updateStage match
            case None =>
            case Some(value) => value.close()
          rootPane.getChildren.remove(rootPane.getCenter)
          val createStage = new Stage{
            title = "new diagram"
            scene = new Scene(1050, 550){
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
              val label = new Label("Added elements")
              label.font = Font.font("Arial",13.0)
              tabulaaaar = Some(new TableView[Diagram]())
              tabulaaaar.get.setMaxWidth(700)
              val name_column = new TableColumn[Diagram, String]("Place")
              name_column.setCellValueFactory(new PropertyValueFactory[Diagram,String]("Place"))

              val interval_column_1 = new TableColumn[Diagram, Int]("From")
              interval_column_1.setCellValueFactory(new PropertyValueFactory[Diagram,Int]("Start"))

              val interval_column_2 = new TableColumn[Diagram, Int]("Until")
              interval_column_2.setCellValueFactory(new PropertyValueFactory[Diagram,Int]("End"))

              val bothIntervalColumn = new TableColumn[Diagram,Int]("Week Index")
              bothIntervalColumn.getColumns.addAll(interval_column_1,interval_column_2)

              val diagram_type_column = new TableColumn[Diagram, String]("Type of Diagram")
              diagram_type_column.setCellValueFactory(new PropertyValueFactory[Diagram,String]("D_type"))
              diagram_type_column.setPrefWidth(diagram_type_column.getPrefWidth + 50.0)

              val measure_diagram_column = new TableColumn[Diagram, String]("Measure")
              measure_diagram_column.setCellValueFactory(new PropertyValueFactory[Diagram,String]("Measure"))

              tabulaaaar.get.getColumns.addAll(name_column,diagram_type_column,bothIntervalColumn,measure_diagram_column)
              tabulaaaar.get.setEditable(true)
              var table_data = FXCollections.observableArrayList[Diagram]()
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

                if diagrams.nonEmpty then
                  val in_start = diagrams.map(x => x.start).min
                  val in_end = diagrams.map(x => x.end).max
                  try{
                    require(first_d.isInstanceOf[Int] && b.isInstanceOf[Int])
                    require(first_d <= b && first_d <= in_end && b >= in_start)
                    require(diagrams.map(x => x.measure).contains(mittari_selector))
                    require(diagrams.map(x => x.D_type).contains(type_of_diag))
                    if kun != null then
                      if !diagrams.map(_.place).contains(kun) then
                        val dg = new Diagram(kun ,"kunta",type_of_diag, first_d,b,mittari_selector,"")
                        diagrams += dg
                        tabulaaaar.get.getItems.addAll(dg)
                      else
                        new Alert(Alert.AlertType.Error,"This place is already in list").showAndWait()
                    else if shp != null then
                      if !diagrams.map(_.place).contains(shp) then
                        val dg = new Diagram(shp ,"shp",type_of_diag, first_d, b, mittari_selector,"")
                        diagrams += dg
                        tabulaaaar.get.getItems.addAll(dg)
                      else
                        new Alert(Alert.AlertType.Error,"This place is already in list").showAndWait()
                    else if kaikki != null && (kaikki == "Kaikki Alueet" || kaikki == "Regions") then
                      if !diagrams.map(_.place).contains("Kaikki Alueet") then
                        val dg = new Diagram("Kaikki Alueet","Kaikki Alueet" ,type_of_diag, first_d, b, mittari_selector, "")
                        diagrams += dg
                        tabulaaaar.get.getItems.addAll(dg)
                      else
                        new Alert(Alert.AlertType.Error,"This place is already in list").showAndWait()
                    else
                      new Alert(Alert.AlertType.Error,"Please choose the place").showAndWait()
                  }
                  catch {
                    case e: IllegalArgumentException =>
                      println("require failed")
                    case _ =>
                      println("other exeption")
                  }

                else
                  try{
                    try{require(first_d.isInstanceOf[Int] && b.isInstanceOf[Int])}catch {case ex: IllegalArgumentException =>new Alert(Alert.AlertType.Error,"There is a problem with your interval").showAndWait()}
                    require(first_d <= b && first_d >= 0 && first_d <= a.getWeek_coodes.last._1 && b >= 0 && b <= a.getWeek_coodes.last._1 - 1)
                    require(shp != null || kun != null || kaikki != null)
                    require(mittari_selector != null && type_of_diag != null)

                    if kun != null then
                      val dg = new Diagram(kun ,"kunta",type_of_diag, first_d,b,mittari_selector,"")
                      diagrams += dg
                      tabulaaaar.get.getItems.addAll(dg)
                    else if shp != null then
                      val dg =  new Diagram(shp ,"shp",type_of_diag, first_d, b, mittari_selector,"")
                      diagrams += dg
                      tabulaaaar.get.getItems.addAll(dg)
                    else if kaikki != null && kaikki == "Kaikki Alueet" || kaikki == "Regions" then
                      val dg = new Diagram("Kaikki Alueet","Kaikki Alueet" ,type_of_diag, first_d, b, mittari_selector, "")
                      diagrams += dg
                      tabulaaaar.get.getItems.addAll(dg)
                    add_remove_savebuttonBox.children.addAll(SaveButton,removeButton)
                  }
                  catch {
                      case e: IllegalArgumentException => println("omg what is happening")
                      case _ => println("no idea")
                    }
                refactor


              SaveButton.onAction = (e: ActionEvent) =>
                if diagrams.nonEmpty then
                  var combined_Data: Buffer[DataPoint] = Buffer[DataPoint]()

                  diagrams.foreach(x => {
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
                  diagrams.head.D_type match
                    case "Bar Chart" =>
                      val pyl = new Pylväs(combined_Data.toArray)
                      pyl.makePic("Bar Diagram","Bar Diagram")
                      rootPane.center = pyl.getPic
                      rootPane.getChildren.addAll(pyl.mover)
                      pyl.clicker()
                      diagramHimself = Some(pyl)
                    case  "Pie Chart"=>
                      val cir = new Circle(combined_Data.toArray)
                      cir.makePic("Pie Chart","Pie Chart")
                      rootPane.center = cir.getPic
                      rootPane.getChildren.addAll(cir.mover)
                      cir.clicker()
                      diagramHimself = Some(cir)
                    case  "Line Chart"=>
                      val viv = new ViivaKaavio(combined_Data.toArray)
                      viv.makePic("Pie Chart","Pie Chart")
                      rootPane.center = viv.getPic
                      rootPane.getChildren.addAll(viv.mover)
                      viv.clicker()
                      diagramHimself = Some(viv)
                else
                  new Alert(Alert.AlertType.Error,"There is no items in list, or previous one was deleted").showAndWait()

              removeButton.onAction = (e: ActionEvent) =>
                if tabulaaaar.get.selectionModel.apply().getSelectedItem != null then
                  val name_place = tabulaaaar.get.selectionModel.apply().getSelectedItem.place
                  diagrams.remove(diagrams.map(x => x.place).indexOf(name_place))
                  tabulaaaar.get.getItems.removeAll(tabulaaaar.get.selectionModel.apply().getSelectedItem)
                else
                  new Alert(Alert.AlertType.Warning,"Please, choose the place to remove").showAndWait()

              tabulaaaar.get.setTranslateY(addButton.getTranslateY + 10)
              val typeBox = new VBox()
              typeBox.children.addAll(typeText,cmobox_3)
              typeBox.setTranslateX(measureSelector.getTranslateX + 60)
              CenterHbox.getChildren.addAll(typeBox)
              cmobox_3.setTranslateY(typeText.getTranslateY + 1)
              dataBox.getChildren.addAll(layout,intervalbox,add_remove_savebuttonBox,tabulaaaar.get)
              dataBox.setTranslateY(dataBox.getTranslateY + 3)
              dataBox.setTranslateX(dataBox.getTranslateX + 3)

              borderPane.left = dataBox
              borderPane.center = CenterHbox
              root = borderPane
            }}
          createStage.show()

        saveDiagram.onAction = (e: ActionEvent) =>
          diagramHimself match
            case None => new Alert(Alert.AlertType.Error, "there is nothing to save at this moment").showAndWait()
            case value => {
              val kunta_interval_start: Map[String,Int] = diagrams.map(x => (x.place,x.start)).toMap
              val kunta_interval_end: Map[String,Int] = diagrams.map(x => (x.place,x.end)).toMap
              val measure_type = diagrams.head.measure
              val diagram_type = diagrams.head.D_type
              val kunta_additional_measures: Map[String, String] = diagrams.map(x => (x.place, x.additional_measures)).toMap

              var kunta_color = Buffer[Kunta_color_changes]()
              // this one saves all colors that was changed
              // in form
              // kaikki alueet  Map(51 -> -fx-background-color: #d8bfd855, 98 -> -fx-background-color: #fffacd)
              // Helsingin ja Uudenmaan SHP  Map(89 -> -fx-background-color: #2f4f4f)
              diagramHimself.get.getData.get.map(_.getData._1).distinct.foreach(x =>{
                var map = Map[Int,String]()
                diagramHimself.get.getData.get.filter(y => y.getData._1 == x).foreach(z =>{

                  z.getColor match
                    case None =>
                    case value =>
                      map += z.getData._2._2.get.toInt -> value.get._1
                })
                if map.nonEmpty then kunta_color += new Kunta_color_changes(x, map)
              })

              var kunta_comment = Buffer[Kunta_comment_changes]()
              diagramHimself.get.getData.get.map(_.getData._1).distinct.foreach(x =>{
                var map = Buffer[Week_Comment]()
                diagramHimself.get.getData.get.filter(y => y.getData._1 == x).foreach(z =>{
                  if z.getComment.nonEmpty then
                    map = map ++ Buffer(new Week_Comment(z.getData._2._2.get.toInt, z.getComment.toList))
                })
                if map.nonEmpty then kunta_comment += new Kunta_comment_changes(x,map.toList)
              })

              val objectForEncode = new FileToWroten(
                diagram_type,measure_type,kunta_interval_start,
                kunta_interval_end,kunta_additional_measures,
                kunta_color.toList,kunta_comment.toList,value.get.getChartName,
                value.get.getAxisName)

              implicit val colorEnc : Encoder[Kunta_color_changes] = deriveEncoder
              implicit val wwwk: Encoder[Week_Comment] = deriveEncoder
              implicit val comment : Encoder[Kunta_comment_changes] = deriveEncoder
              implicit val fileToWroten: Encoder[FileToWroten] = deriveEncoder

              try{
                var name = ".txt"
                val filename = new Stage{
                  scene = new Scene(200,200){
                    val textf = new TextField()
                    textf.setPromptText("type the name of file")
                    val saveb = new Button("Save")
                    saveb.onAction = (e: ActionEvent) =>
                      if textf.getText != null && textf.getText != "" then
                        name = textf.getText + name
                        val file = new File("src/main/scala/savedTxtFiles/" + name)
                        val writer = new BufferedWriter(new FileWriter(file))
                        val data_to_Store = fileToWroten.apply(objectForEncode).toString
                        writer.write(data_to_Store)
                        writer.close()
                    val borP = new BorderPane()
                    val tx = new Text("Please enter the file's name without txt, commas etc.")
                    tx.font = Font.font(12.0)
                    borP.center = new VBox(tx,textf,saveb)
                    root = borP
                  }
                }
                filename.show()
              }
              catch {
                case e: IOException =>
                  new Alert(Alert.AlertType.Error,"The name of the file is broken, file\nwill be wroten to out.txt").showAndWait()
                  val file = new File("src/main/scala/savedTxtFiles/out.txt")
                  val writer = new BufferedWriter(new FileWriter(file))
                  val data_to_Store = fileToWroten.apply(objectForEncode).toString
                  writer.write(data_to_Store)
                  writer.close()
              }

            }

        UpdateDiagram.onAction = (e: ActionEvent) =>
          if diagrams.nonEmpty then
            updateStage = Some(new AfterWordMaintainer)
            updateStage.get.show()
          else
            new Alert(Alert.AlertType.Error,"Choose diagram first").showAndWait()

        compareItem.onAction = (e: ActionEvent) =>
          new AdditionalStage().show()

        fileItem.onAction = (e: ActionEvent) =>
          var fileToBeOppened = "src/main/scala/savedTxtFiles/"
          var tobeJasoned = ""
          val openStage = new Stage{
            val textField = new TextField()
            title = "Input file's name"
            val opener_bp = new BorderPane()
            scene = new Scene(300, 200){
              textField.setPromptText("Type last prefix of file with .txt")
              val openButton = new Button("Open")
              openButton.setTranslateY(textField.getTranslateY + 3)
              val container = new VBox(textField,openButton)
              container.setTranslateY(container.getTranslateY + 10)
              openButton.onAction = (ac: ActionEvent) =>
                val f = new File(fileToBeOppened + textField.getText)
                if f.isFile then
                  implicit val colorDec : Decoder[Kunta_color_changes] = deriveDecoder
                  implicit val wwwkDec: Decoder[Week_Comment] = deriveDecoder
                  implicit val commentDec : Decoder[Kunta_comment_changes] = deriveDecoder
                  implicit val fileToWrotenDec: Decoder[FileToWroten] = deriveDecoder
                  try{
                    val reader = new BufferedReader(new FileReader(f))
                    var oneLine = reader.readLine()
                    if oneLine!= null then tobeJasoned = tobeJasoned + oneLine
                    while oneLine != null do
                      oneLine = reader.readLine()
                      if oneLine!= null then tobeJasoned = tobeJasoned + oneLine
                    val decodded = parser.decode[FileToWroten](tobeJasoned).toTry.get
                    val u = a.getShp_kunnat.map(_._2)
                    raspakouka(decodded)
                  }
                  catch{
                    case  e: IOException => new Alert(Alert.AlertType.Error, "File does not exists").showAndWait()
                    }

                else
                  new Alert(Alert.AlertType.Error,"Please check your input").showAndWait()
              opener_bp.center = container
              root = opener_bp
            }

          }

          openStage.show()

        def raspakouka(file: FileToWroten)=
          val diagram_type = file.dg_type
          val place_type_code = file.in_start.keys.map(x =>{
            val shpT = a.getShp_kunnat.map(_._1)
            val kunnat = a.getShp_kunnat.flatMap(_._2).map(_._2)
            if kunnat.contains(x) then
              (x,"kunta")
            else if shpT.contains(x) then
              (x,"shp")
            else
              (x,"Kaikki Alueet")
          }).map(y => {
            val shpt = a.getTriplat
            val kunnat = a.getShp_kunnat.flatMap(_._2)
            if shpt.map(_._3).contains(y._1) then
              val index = shpt.map(_._3).indexOf(y._1)
              (y._1,y._2,shpt(index)._2)
            else if kunnat.map(_._2).contains(y._1) then
              val index = kunnat.map(_._2).indexOf(y._1)
              (y._1,y._2,kunnat(index)._1)
            else
              (y._1,y._2,all_regions)
          })
          var combined_Data: Buffer[DataPoint] = Buffer[DataPoint]()
          diagrams = Buffer[Diagram]()
          diagramHimself = None
          tabulaaaar = Some(new TableView[Diagram]())
          tabulaaaar.get.setMaxWidth(700)
          val name_column = new TableColumn[Diagram, String]("Place")
          name_column.setCellValueFactory(new PropertyValueFactory[Diagram,String]("Place"))

          val interval_column_1 = new TableColumn[Diagram, Int]("From")
          interval_column_1.setCellValueFactory(new PropertyValueFactory[Diagram,Int]("Start"))

          val interval_column_2 = new TableColumn[Diagram, Int]("Until")
          interval_column_2.setCellValueFactory(new PropertyValueFactory[Diagram,Int]("End"))

          val bothIntervalColumn = new TableColumn[Diagram,Int]("Week Index")
          bothIntervalColumn.getColumns.addAll(interval_column_1,interval_column_2)

          val diagram_type_column = new TableColumn[Diagram, String]("Type of Diagram")
          diagram_type_column.setCellValueFactory(new PropertyValueFactory[Diagram,String]("D_type"))
          diagram_type_column.setPrefWidth(diagram_type_column.getPrefWidth + 50.0)

          val measure_diagram_column = new TableColumn[Diagram, String]("Measure")
          measure_diagram_column.setCellValueFactory(new PropertyValueFactory[Diagram,String]("Measure"))

          tabulaaaar.get.getColumns.addAll(name_column,diagram_type_column,bothIntervalColumn,measure_diagram_column)
          tabulaaaar.get.setEditable(true)

          rootPane.getChildren.remove(rootPane.getCenter)
          place_type_code.foreach(x => {
            val mit = mittariDefiner(file.measure_key)
            val mit_2 = if mit == case_measure_code then tested_measure_code else case_measure_code
            x._2 match
              case "kunta" =>
                val toBeStored = a.kunta_Creator((file.in_start(x._1),file.in_end(x._1)),Array(x._3))
                val dg = new Diagram(x._1 ,"kunta",diagram_type, file.in_start(x._1),file.in_end(x._1),file.measure_key,"")
                diagrams += dg
                tabulaaaar.get.getItems.addAll(dg)
                combined_Data = combined_Data ++ toBeStored.toBuffer
              case "shp" =>
                val tobeStored = a.SHP_creator_week((file.in_start(x._1),file.in_end(x._1)),Array(x._3),Array(mit,mit_2))
                val dg = new Diagram(x._1 ,"shp",diagram_type, file.in_start(x._1),file.in_end(x._1),file.measure_key,"")
                diagrams += dg
                tabulaaaar.get.getItems.addAll(dg)
                combined_Data = combined_Data ++ tobeStored.toBuffer
              case "Kaikki Alueet" =>
                val mit_3 = Array(mit) ++ Array(case_measure_code,tested_measure_code,death_measure_code,other_death,defined_death_because_of_corona).filter(_ != mit)
                val tobeStored = a.kaikki_alueet_viikko_mittari((file.in_start(x._1),file.in_end(x._1)), mit_3)
                val dg = new Diagram(x._1 ,"Kaikki Alueet",diagram_type, file.in_start(x._1),file.in_end(x._1),file.measure_key,"")
                diagrams += dg
                tabulaaaar.get.getItems.addAll(dg)
                combined_Data = combined_Data ++ tobeStored.toBuffer
              case _ => println("Kaikki Alueet")
          })
          diagram_type match
            case "Bar Chart" =>
              val combinedData_colorized = chartColorinitialazer(file,combined_Data.toArray)
              val pyl = new Pylväs(combinedData_colorized)
              pyl.makePic("Bar Chart","Bar Chart")
              rootPane.center = pyl.getPic
              rootPane.getChildren.addAll(pyl.mover)
              pyl.clicker()
              pyl.changeAxisName(file.axisName)
              pyl.changeName(file.charName)
              diagramHimself = Some(pyl)
            case  "Pie Chart"=>
              val combinedData_colorized = chartColorinitialazer(file,combined_Data.toArray)
              val cir = new Circle(combinedData_colorized)
              cir.makePic("Pie Chart","Pie Chart")
              rootPane.center = cir.getPic
              rootPane.getChildren.addAll(cir.mover)
              cir.clicker()
              cir.changeAxisName(file.axisName)
              cir.changeName(file.charName)
              diagramHimself = Some(cir)
            case  "Line Chart"=>
              val combinedData_colorized = chartColorinitialazer(file,combined_Data.toArray)
              val viv = new ViivaKaavio(combinedData_colorized)
              viv.makePic("Line Chart","Line Chart")
              rootPane.center = viv.getPic
              rootPane.getChildren.addAll(viv.mover)
              viv.clicker()
              viv.changeAxisName(file.axisName)
              viv.changeName(file.charName)
              diagramHimself = Some(viv)

        def chartColorinitialazer(file: FileToWroten, diagram: Array[DataPoint]) =
          file.kunta_color_changes.foreach(x =>{
            val name = x.name
            val color_week_tuples = x.week_color.toArray.map(y => (y._1,y._2,Color.web("#" + y._2.split("#").apply(1))))
            color_week_tuples.foreach(x => diagram.filter(_.getData._1 == name).filter(_.getData._2._2.get == x._1.toString).head.changeColor((x._2,x._3)))
          })
          file.kunta_comment_changes.foreach(x => {
            val comments_week_tuples = file.kunta_comment_changes.filter(y => y.name == x.name).map(z => z.week_comment).head
            comments_week_tuples.foreach(y => y.comments.foreach(i=> diagram.filter(z => z.getData._1 == x.name && z.getData._2._2.get == y.week.toString).head.addComment(i)))
          })
          diagram



        Analyze.onAction = (ev: ActionEvent) =>
          diagramHimself match
            case None =>
              new Alert(Alert.AlertType.Information,"There is no diagram to analyze").showAndWait()
            case Some(value) =>
              val analyzeStage = new AnalyzeStage(diagrams.toArray,value.getData.get.toArray)
              analyzeStage.show()


        root = rootPane
     }
    }
   }
   def getDiagramHimself = diagramHimself
   def setDiagramHimself(board: Option[StatisticBoardPoints]) =
     diagramHimself = board

   def getDiagrams = diagrams
   def setDiagrams(grams: Buffer[Diagram]): Unit =
     diagrams = grams

   def getRootPane = rootPane

   def getTabulaaaar = tabulaaaar
   def setTabulaaaar(tbl: TableView[Diagram])=
     tabulaaaar


   /** helper function to define the code of the key
    * while initializtion during program running */

   def mittariDefiner(mittari: String) =
     mittari match
       case "1" => case_measure_code
       case "2" => tested_measure_code
       case "3" => death_measure_code
       case "4" => defined_death_because_of_corona
       case "5" => other_death



}
