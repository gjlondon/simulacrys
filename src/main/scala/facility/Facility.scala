package facility

import entity.Entity
import message.Message

import scala.collection.immutable.Queue

sealed trait Facility extends Entity {
  val name: String = this.getClass.getSimpleName
  val capacity: Int

  def reserve: Facility
  def release: Facility
  val isAvailable: Boolean = capacity >= 1
}

case class Pasture(capacity: Int = 3) extends Facility {
  override def reserve: Pasture = this.copy(capacity = capacity - 1)

  override def release: Pasture = this.copy(capacity = capacity + 1)

  /** base trait for any "object" in the world that is capable of acting or being acted on
    * e.g. people, animals, chairs.
    *
    * Each will have a mailbox where they receive incoming messages which they will
    * process and respond to before taking an "voluntary" actions
    */
  override val inbox: Queue[Message[Entity, Entity, Entity]] = ???
  override val outbox: Queue[Message[Entity, Entity, Entity]] = ???
}

case class Farm(capacity: Int = 2) extends Facility {
  override def reserve: Farm = this.copy(capacity = capacity - 1)

  override def release: Farm = this.copy(capacity = capacity + 1)
}

case class Forest(capacity: Int = 1) extends Facility {
  override def reserve: Forest = this.copy(capacity = capacity - 1)

  override def release: Forest = this.copy(capacity = capacity + 1)
}

sealed trait FacilityGroup

case object Farms extends FacilityGroup
case object Pastures extends FacilityGroup
case object Forests extends FacilityGroup