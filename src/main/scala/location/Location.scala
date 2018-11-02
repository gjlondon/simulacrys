package location

import populace.Populace

sealed trait Location {
  val name: String
  val populace: Populace
  def withNewPopulace(populace: Populace): Location  // TODO maybe replace with a lens?
}

case class City(name: String, populace: Populace) extends Location {
  override def withNewPopulace(populace: Populace): Location = this.copy(populace = populace)
}

case class Farm(name: String, populace: Populace) extends Location {
  override def withNewPopulace(populace: Populace): Location = this.copy(populace = populace)
}
