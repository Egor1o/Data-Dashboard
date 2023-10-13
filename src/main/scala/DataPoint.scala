import scalafx.scene.paint.Color


import java.time.*
import java.util.Date
import scala.collection.mutable.*

class DataPoint(place: String, date: (Option[String],Option[String]), measure: String, cases: String) {
  private var additionalData: Buffer[String] = Buffer()
  private var color: Option[(String,Color)] = None
  private var tyyppi = ""
  private var visibility: Boolean = true
  private var data = (place,date,measure,cases)
  
  def changeColor(css: (String,Color)): Unit =
    color = Some(css)
  def addComment(data: String): Unit = additionalData += data
  def getComment = additionalData
  def changeVisbility(): Unit = visibility = !visibility
  def getColor: Option[(String,Color)] = color
  def getType: String = measure
  def getDate: (Option[String],Option[String]) = date
  def getData: (String,(Option[String],Option[String]),String,String) =
    data
  override def toString: String = getData._1
  

}
