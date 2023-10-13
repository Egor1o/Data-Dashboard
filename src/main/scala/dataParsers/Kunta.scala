package dataParsers


case class Kunta(dataset: BaseInfo)

case class BaseInfo(version: String, klass: String, label: String, dimension: Dim, value: Map[String,String])

case class Dim(id: Vector[String], size: Vector[Int],
                     hcdmunicipality2020: Viikko,
                     dateweek20200101: Viikko, measure: Measur,
                     )

case class Viikko(category: Categorr)

case class Categorr(index: Map[String,Int], label: Map[String,String])

case class Measur(category: Categorr)