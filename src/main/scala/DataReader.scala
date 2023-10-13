import io.circe.Json
import io.circe.syntax.*
import io.circe.Decoder
import io.circe.*
import io.circe.generic.semiauto.*
import dataParsers.*
import scala.collection.mutable.Buffer
import scala.util.{Failure, Success, Try}

class DataReader {
  
  private val start_link = "https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?"

  /** default adders to link */
  private val place = "row=hcdmunicipality2020-"
  private val place_1 = "hcdmunicipality2020"
  private val dataweek = "column=dateweek20200101-"
  private val dataweek_1 = "dateweek20200101"
  private val measure = "column=measure-"

  /** common codes for default links */
  private val all_regions = "445222"
  private val all_times = "509030"

  /** measures codes */
  private val death_measure_code = "492118"
  private val case_measure_code = "444833"
  private val tested_measure_code = "445356"
  private val defined_death_because_of_corona = "816930"
  private val other_death = "816957"

  private val week_coodes = get_availible_weeks_days(
    dataParser(datanSaanti("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?row=dateweek20200101-509030")).toTry.get,"dateweek20200101")
  def getWeek_coodes = week_coodes

  private val week_codes_mapped_by_codes = week_coodes.map(x => x._2)

  private val SHPs = get_availible_weeks_days(
    dataParser(datanSaanti("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?" +
      "row=hcdmunicipality2020-445222&column=dateweek20200101-508933&column=measure-444833")).toTry.get,"hcdmunicipality2020")
  private val SHP_names = get_availible_names(
    dataParser(datanSaanti("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?" +
      "row=hcdmunicipality2020-445222&column=dateweek20200101-508933&column=measure-444833")).toTry.get)
  def getSHP_names = SHP_names

  /**
      (0,445131,Ahvenanmaa)
      (1,445197,Varsinais-Suomen SHP)
      ...
      gives tuples of(index of SHP, code of SHP, name of SHP)
   */
  private val triplat = SHPs.map(x =>
    {
      val a = SHP_names.filter(y => y._1 == x._2)
      (x._1,x._2,a.head._2)
    })
  def getTriplat = triplat

  private val shp_kunnat: Buffer[(String, Buffer[(String,String)])] = {

    val codes = SHP_names.filter(_._2 != "Kaikki Alueet")
    var buf: Buffer[(String,Buffer[(String,String)])] = Buffer[(String,Buffer[(String,String)])]()
    codes.foreach(x => {

      val names = get_availible_names(dataParser(datanSaanti("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?row=hcdmunicipality2020-" + x._1)).toTry.get)
      buf += x._2 -> names.filter(y => y._2 != x._2)

    })
    buf

  }
  def getShp_kunnat = shp_kunnat

  /** Gets data from url and changes to String */
  def datanSaanti(url: String): String =
    scala.io.Source.fromURL(url).getLines().mkString("\n")

  /** Parsing of data */
  def dataParser(data: String): Either[ParsingFailure, Json] =
    parser.parse(data)
  
  
  implicit val kuntaDecoder: Decoder[Kunta] = deriveDecoder


  /** kuntaDecoder for creation of KuntaInstance */
  def kuntaData(data: Json): Kunta =
    var rewriteData = Right(data.hcursor.downField("dataset").withFocus(x => CirceUtil.renameField(x, "class", "klass")).top.get)
    parser.decode[Kunta](rewriteData.toTry.get.toString).toTry.get

 /** returns all regions/cities and their code's in reverse order */
  private def region_week_spotter(data: Json, row: String): Map[String, Int] =
    val region_ids = data.hcursor.downField("dataset").downField("dimension").downField(row)
      .downField("category")
    val names_indexes = region_ids.downField("label").focus.get.as[Map[String,String]]
    names_indexes.toTry.get.map(x => (x._2,x._1.toInt))

  /** Unique method for searching code of the week wtih description
   * or code of city with the higher description of region 
   * example 1 : Some((Vuosi 2021 Viikko 06,509181,true))
   * example 2 : Some((Lapin SHP,444999,true)) */
  private def place_week_informer(name: String, row: String, row_2: String, code: String) : Option[(String,Int,Boolean)] =
    var all_places = Buffer[(String,Map[String,Int])]()
    for region <- region_week_spotter(dataParser(datanSaanti(start_link + row + code)).toTry.get, row_2) do
      val new_link = start_link + row + region._2
      val new_data = dataParser(datanSaanti(new_link)).toTry.get
      all_places += (region._1 -> region_week_spotter(new_data, row_2))
    val fiasco = all_places.map(x => x._2.keys.exists(y => y == name))
    if fiasco.contains(true) then
      val ind = fiasco.indexOf(true)
      Some((all_places(ind)._1,all_places(ind)._2(name),true))
    else
      Some(("Not defined",0,false))

  /** initialazing week-codes */
  def get_availible_weeks_days (data: Json, tunniste: String) =
    data.hcursor.downField("dataset").downField("dimension").downField(tunniste)
      .downField("category").downField("index").as[Map[String,Int]].toTry.get.toBuffer
      .map(x => (x._2,x._1)).sorted

  def get_availible_names (data: Json) =
    data.hcursor.downField("dataset").downField("dimension").downField("hcdmunicipality2020")
      .downField("category").downField("label").as[Map[String,String]].toTry.get.toBuffer


  /** defines interval between two datesdfr */
  def weekInterval(start: String, end: String): (Option[String], Option[String]) =
    var st: Option[String] = None
    var en: Option[String] = None
    var count = 0
    for week <- week_coodes do
      val a = dataParser(datanSaanti(start_link + "column=dateweek20200101-" + week._2))
        .toTry.get.hcursor.downField("dataset").downField("dimension").downField("dateweek20200101")
        .downField("category")
      val b = a.downField("label").as[Map[String,String]].toTry.get
      val c = b.filter(x =>{x._2 == start || x._2 == end})
      if c.size == 2 then
        val returnable = a.downField("index").as[Map[String,Int]].toTry.get.filter(x => x._2 == 7)
        return (Some(returnable.head._1),Some(returnable.head._1))
      else if c.size == 1 && count == 0 then
        st = Some(a.downField("index").as[Map[String,Int]].toTry.get.filter(x => x._2 == 7).head._1)
        count += 1
      else if c.size == 1 && count == 1 then
        en = Some(a.downField("index").as[Map[String,Int]].toTry.get.filter(x => x._2 == 7).head._1)
        count += 1
      if count == 2 then return (st,en)
    (st,en)

  /** creates dataPoints according to the week statistics for kunta and SHP
   * parametrs (parsed kunta, interval, keys of kunta/Shp in the given order
   * informs only about corona-cases by 1 week*/
  def kunta_Creator(interval: (Int,Int), keys_in_order: Array[String]): Array[DataPoint] =
    /** if data was taken from file, fromFile will consists some data */
    val kunta = kuntaData(dataParser(datanSaanti(start_link + place + keys_in_order.mkString(".") + "&" + dataweek + all_times
    + "&" + measure + case_measure_code)).toTry.get)
    val kunnat = kunta.dataset.dimension.hcdmunicipality2020.category.label.toArray
    val order = keys_in_order.map(x => kunnat.filter(y => y._1 == x).head)
    var points = Buffer[Option[DataPoint]]()
    for i <- interval._1 to interval._2 do
      for place <- order do
        if order.indexOf(place) == 0 then
          points += apu_kunta_creator(i,kunta,place,i.toString)
        else
          val a = week_coodes.takeRight(1).head._1 * order.indexOf(place) + i + 1
          points += apu_kunta_creator(a,kunta,place,i.toString)
    points.map(_.get).toArray

  /** initializer helper function for kunta_Creator */
  def apu_kunta_creator(a : Int, kunta: Kunta, place : (String,String), b: String) =
    if kunta.dataset.value.keys.exists(x => x == a.toString) then
      var tobeSetted =
        if kunta.dataset.value(a.toString) == ".." then
          "0"
        else
          kunta.dataset.value(a.toString)
      Some(new DataPoint(place._2,(None,Some(a.toString)),"corona cases",tobeSetted))
    else
      Some(new DataPoint(place._2,(None,Some(a.toString)),"corona cases","0"))

  /** data frowm week SHP with tartunta and testausmäärä */
  def SHP_creator_week(interval: (Int, Int), keys_in_order: Array[String], measure_keys: Array[String]): Array[DataPoint] =
    try {
      require(interval._1 >= 0 && interval._2 <= week_coodes.takeRight(1).head._1 && measure_keys.length >= 0
      && measure_keys.length <= 2
      && measure_keys.forall(x => x == tested_measure_code || x == case_measure_code))
      val measure_transcript = mittari_transcripted(mittariDefiner(measure_keys))
      val shpt = kuntaData(
      dataParser(datanSaanti(start_link + place + all_regions + "&" +
        dataweek + all_times + "&" + measure + measure_keys.mkString("."))).toTry.get)
      val valuet = shpt.dataset.value
      val kunnat = triplat.filter(x => keys_in_order.contains(x._2))
      var points = Buffer[Option[DataPoint]]()
      for i <- interval._1 to interval._2 do
        for place <- kunnat do
          var laskelma_mittarin_mukaan: String = ""
          if place._1 == 0 then
            val index = i * 2
            laskelma_mittarin_mukaan += SHP_apu(valuet, measure_keys.length, index)
          else
            val index = ((week_coodes.reverse.head._1 + 1) * 2) * (place._1) + (i * 2)
            laskelma_mittarin_mukaan += SHP_apu(valuet, measure_keys.length, index)
          points += Some(new DataPoint(place._3, (None, Some(i.toString)),
            measure_transcript, laskelma_mittarin_mukaan))
      points.map(_.get).toArray
    }
    catch {
      case e: IllegalArgumentException =>
        println("Shp_creator_2")
        Buffer[DataPoint]().toArray
      case _ =>
        println("SHP_creator_3")
        Buffer[DataPoint]().toArray
    }

  /** helper function for shp creator to give proper values for the point */
  def SHP_apu(valueMap: Map[String,String], len: Int, index:Int)=
    var i = 0
      var valueOfPoint = ""
      try{
      for j <- 0 until len do
        if j == 0 then
          valueOfPoint += valueMap(index.toString)
        else
          valueOfPoint += " - " + valueMap((index + 1).toString)
        i += 1
      }
      catch {
        case e: NoSuchElementException =>
          if i == 0 then valueOfPoint += "0"
          else valueOfPoint += " - " + "0"
        case _ => println("Some problem occured in this calculation")
      }
      valueOfPoint


  /** takes statistics from all regions by week-interval
   * accordting to the measures defined in: mittarit */
  def kaikki_alueet_viikko_mittari(interval: (Int, Int), mittarit: Array[String]): Array[DataPoint] =
    require(mittarit.length >= 1)
    var link = start_link + place + all_regions + "&" + dataweek + all_times + "&" + measure + mittarit.head
    mittarit.takeRight(mittarit.length - 1).foreach(x => link = link + "." + x)
    val kunta = kuntaData(dataParser(datanSaanti(link)).toTry.get)
    var points = Buffer[Option[DataPoint]]()
    for i <- interval._1 to interval._2 do
      var laskelma = ""
      for m <- mittarit.indices do
        val index = ((week_coodes.reverse.head._1 + 1) * mittarit.length) * (21) + (i * mittarit.length) + m
        if m > 0 then laskelma += " - "
        try {
          laskelma += kunta.dataset.value(index.toString)
        }
        catch {
          case e: NoSuchElementException =>
            laskelma += "0"
          case _ => laskelma += "Some problem occured in this calculation"
        }
      points += Some(new DataPoint("kaikki alueet", (None, Some(i.toString)), mittari_transcripted(mittariDefiner(mittarit)), laskelma))
    points.map(_.get).toArray


  def mittariDefiner(mittarit: Array[String]): Array[String]=
    var decoded_names = Array[String]()
    for mittari <- mittarit do
      mittari match
        case "492118" => decoded_names = decoded_names ++ Array("deathes")
        case "444833" => decoded_names = decoded_names ++ Array("corona cases")
        case "445356" => decoded_names = decoded_names ++ Array("tests")
        case "816930" => decoded_names = decoded_names ++ Array("death because of corona")
        case "816957" => decoded_names = decoded_names ++ Array("other_deathes")
        case _ =>
    try {
      require(mittarit.length == decoded_names.length)
    }
    catch {
      case e: IllegalArgumentException =>
        println("Not_defined_length")
        Array[String]()
      case _ =>
        println("other exception in mittari")
        Array[String]()
    }
    decoded_names

  /** helper function to get String of keys*/
  def mittari_transcripted(mittarit: Array[String])=
    mittarit.mkString(" - ")

  /** tells where the place belongs to */
  def where_belongs(name: String) =
    val paikka = place_week_informer(name,place,place_1,all_regions).get
    if paikka._3 then
      if paikka._1.toLowerCase == "kaikki alueet" then
        ("SHP", paikka._2)
      else
        ("kunta",paikka._2)
    else
      ("not defined", 0)
}

/** Object created to rename Json object's field */
object CirceUtil {
  def renameField(json: Json, fieldToRename: String, newName: String): Json =
    (for {
      value <- json.hcursor.downField(fieldToRename).focus
      newJson <- json.mapObject(_.add(newName, value)).hcursor.downField(fieldToRename).delete.top
    } yield newJson).getOrElse(json)
}