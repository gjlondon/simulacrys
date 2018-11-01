package location

sealed trait Location {
  val name: String
}

case class City(name: String) extends Location
case class Farm(name: String) extends Location
