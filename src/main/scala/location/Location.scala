package location

import populace.Populace

sealed trait Location {
  val name: String
  val populace: Populace
}

case class City(name: String, populace: Populace) extends Location
case class Farm(name: String, populace: Populace) extends Location
