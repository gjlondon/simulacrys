package facility

import entity.Entity
import message.MailboxTypes.{Inbox, Outbox}
import message.{Mailbox, Message}

import scala.collection.immutable.Queue

sealed trait Facility extends Entity {
  val name: String = this.getClass.getSimpleName
  val capacity: Int
  val grouping: FacilityGroup
  val isAvailable: Boolean = capacity >= 1

  def reserve: Facility
  def release: Facility
  def receiveMessages(messages: Queue[Message]): Facility

}

case class Pasture(capacity: Int = 3,
                   inbox: Inbox = Mailbox.empty,
                   outbox: Outbox = Mailbox.empty)
  extends Facility {
  val grouping: Pastures.type = Pastures
  override def reserve: Pasture = this.copy(capacity = capacity - 1)

  override def release: Pasture = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Pasture = {
    this.copy(inbox = inbox ++ messages)
  }
}

case class Farm(capacity: Int = 2,
                inbox: Inbox = Mailbox.empty,
                outbox: Outbox = Mailbox.empty)
  extends Facility {
  val grouping: Farms.type = Farms
  override def reserve: Farm = this.copy(capacity = capacity - 1)

  override def release: Farm = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Farm = {
    this.copy(inbox = inbox ++ messages)
  }
}

case class Forest(capacity: Int = 1, inbox: Inbox = Mailbox.empty,
                  outbox: Outbox = Mailbox.empty)
  extends Facility {
  val grouping: Forests.type = Forests

  override def reserve: Forest = this.copy(capacity = capacity - 1)

  override def release: Forest = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Forest = {
    this.copy(inbox = inbox ++ messages)
  }
}

sealed trait FacilityGroup

case object Farms extends FacilityGroup
case object Pastures extends FacilityGroup
case object Forests extends FacilityGroup