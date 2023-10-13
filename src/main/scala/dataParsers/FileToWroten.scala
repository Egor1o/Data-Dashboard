package dataParsers

case class FileToWroten(dg_type : String,measure_key: String,in_start : Map[String,Int],in_end: Map[String,Int],
                        add_mes : Map[String, String],kunta_color_changes: List[Kunta_color_changes],
                        kunta_comment_changes: List[Kunta_comment_changes],charName: String,
                        axisName: String)

case class Kunta_color_changes(name: String, week_color: Map[Int,String])
case class Week_Comment(week: Int, comments: List[String])
case class Kunta_comment_changes(name: String, week_comment: List[Week_Comment])
