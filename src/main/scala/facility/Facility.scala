package facility

sealed trait Facility {
  val name: String = this.getClass.getSimpleName
  val capacity: Int
}

case class Pasture(capacity: Int = 3) extends Facility
case class Farm(capacity: Int = 2) extends Facility
case class Forest(capacity: Int = 1) extends Facility

sealed trait FacilityGroup

case object Farms extends FacilityGroup
case object Pastures extends FacilityGroup
case object Forests extends FacilityGroup