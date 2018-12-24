package message

import actions.Interaction
import entity.Entity
import facility.Facility
import person.Commoner

import scala.collection.immutable.Queue

sealed trait Message[+T <: Entity, U <: Entity, +V <: Entity] {
  val from: T
  val to: U
  val payload: Interaction[T, U, V]
}

case class PersonToFacilityMessage(from: Commoner, to: Facility,
                                   payload: Interaction[Commoner, Facility, Facility])
  extends Message[Commoner, Facility, Facility]

object MailboxTypes {
  type Inbox = Mailbox
  type Outbox = Mailbox
  type Mailbox = Queue[Message[Entity, Entity, Entity]]
}

object Mailbox {
  import MailboxTypes._

  def empty: Mailbox = {
    Queue[Message[Entity, Entity, Entity]]()
  }
}