package facility

import entity.Entity
import message.Mailbox
import message.MailboxTypes.{Inbox, Outbox}

sealed trait Facility extends Entity {
  val name: String = this.getClass.getSimpleName
  val capacity: Int

  def reserve: Facility
  def release: Facility
  val isAvailable: Boolean = capacity >= 1
}

case class Pasture(capacity: Int = 3,
                   inbox: Inbox = Mailbox.empty,
                   outbox: Outbox = Mailbox.empty)
  extends Facility {
  override def reserve: Pasture = this.copy(capacity = capacity - 1)

  override def release: Pasture = this.copy(capacity = capacity + 1)
}

case class Farm(capacity: Int = 2,
                inbox: Inbox = Mailbox.empty,
                outbox: Outbox = Mailbox.empty)
  extends Facility {
  override def reserve: Farm = this.copy(capacity = capacity - 1)

  override def release: Farm = this.copy(capacity = capacity + 1)
}

case class Forest(capacity: Int = 1, inbox: Inbox = Mailbox.empty,
                  outbox: Outbox = Mailbox.empty)
  extends Facility {
  override def reserve: Forest = this.copy(capacity = capacity - 1)

  override def release: Forest = this.copy(capacity = capacity + 1)
}

sealed trait FacilityGroup

case object Farms extends FacilityGroup
case object Pastures extends FacilityGroup
case object Forests extends FacilityGroup