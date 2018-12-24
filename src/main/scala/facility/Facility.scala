package facility

trait Facility {
  val facilityType: String = this.getClass.getSimpleName
  val capacity: Int
}

case class Pasture(capacity: Int = 3) extends Facility
case class Farm(capacity: Int = 2) extends Facility
case class Forest(capacity: Int = 1) extends Facility
