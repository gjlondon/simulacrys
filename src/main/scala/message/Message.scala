package message

import java.util.UUID
import java.util.UUID.randomUUID

import entity.Entity

import scala.collection.immutable.Queue


sealed trait Message {
  val uuid: UUID = randomUUID()
  val from: UUID
  val to: UUID
}

case class Request[U <: Entity](from: UUID, to: UUID,
                      condition: U => Boolean,
                      onSuccess: U => U,
                      onFailure: U => U) extends Message

case class Reply(re: UUID,
                 from: UUID,
                 to: UUID,
                 succeeded: Boolean) extends Message


object MailboxTypes {
  type Inbox = Mailbox
  type Outbox = Mailbox
  type Mailbox = Queue[Message]
}

object Mailbox {
  import MailboxTypes._

  def empty: Mailbox = {
    Queue[Message]()
  }

  def from(message: Message): Mailbox = {
    Queue[Message](message)
  }
}