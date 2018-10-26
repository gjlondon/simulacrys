package location

sealed trait Location {
  val name: String
  val latitude: Double
  val longitude: Double
}

case class City(name: String, latitude: Double, longitude: Double) extends Location
