import javafx.event.ActionEvent
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.VBox
import scalafx.scene.control.Button
import scalafx.scene.control.TextField
import scalafx.stage.Stage
import scalafx.scene.*
class ChangeAxisNameStage(board: StatisticBoardPoints, toBeChanged: String) extends Stage{
  title = "change name"
  scene = new Scene(250,250){
    val text = new TextField()
    val saveButton = new Button("Save new name")
    val vbox = new VBox(text,saveButton)
    saveButton.onAction = (a: ActionEvent) =>
      if text.getText != null && text.getText != "" && toBeChanged == "axis" then
        board.changeAxisName(text.getText)
      else if text.getText != null && text.getText != "" && toBeChanged == "name" then
        board.changeName(text.getText)
    val bp = new BorderPane()
    bp.center = vbox
    root = bp
  }
}
