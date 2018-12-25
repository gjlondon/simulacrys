package message

import java.util.UUID

import actions.Interaction
import entity.Entity
import facility.Facility
import person.Commoner
import java.util.UUID.randomUUID

import scala.collection.immutable.Queue
import scala.reflect.ClassTag

sealed trait Message[+T <: Entity, U <: Entity, +V <: Entity] {
  val from: T
  val to: U
  val payload: Interaction[T, U, V]
}

case class Msg[+T: ClassTag, +U: ClassTag](from: T, to: U,
                      condition: PartialFunction[AnyRef, Boolean],
                      onSuccess: PartialFunction[AnyRef, Option[U]],
                      onFailure: PartialFunction[AnyRef, Option[U]],
                      success: Option[Boolean] = None,
                     )

sealed trait Note[+T, +U] {
  val uuid: UUID = randomUUID()
  val from: T
  val to: U
}

case class NoteRequest[+T, U](from: T, to: U,
                              condition: U => Boolean,
                              onSuccess: U => U,
                              onFailure: U => U) extends Note[T, U]

case class NoteReply[T: ClassTag, U: ClassTag](re: UUID,
                                               from: T,
                                               to: U,
                                               succeeded: Boolean) extends Note[T, U]



sealed trait ReqRep

case class Request[+T, U](from: T, to: U,
                          condition: U => Boolean,
                          effect: U => U,
                          onSuccess:PartialFunction[AnyRef, Option[T]],
                          onFailure: PartialFunction[AnyRef, Option[T]]) extends ReqRep

case class Reply[T: ClassTag, U: ClassTag](from: T, to: U, succeeded: Boolean,
                       onSuccess:U => U, onFailure: U => U) extends ReqRep

case class PersonToFacilityMessage(from: Commoner, to: Facility,
                                   payload: Interaction[Commoner, Facility, Facility])
  extends Message[Commoner, Facility, Facility]

case class PersonNoOp(from: Commoner, to: Commoner,
                                   payload: Interaction[Commoner, Commoner, Commoner])
  extends Message[Commoner, Commoner, Commoner]


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