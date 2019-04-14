package com.thrive.simulation.message

import java.util.UUID
import java.util.UUID.randomUUID

import scala.collection.immutable.Queue

sealed trait MessagePayload

case object NoOp extends MessagePayload
case object Reserve extends MessagePayload
case object Release extends MessagePayload

sealed trait Message {
  val uuid: UUID = randomUUID()
  val from: UUID
  val to: UUID
}

case class Request(from: UUID, to: UUID, payload: MessagePayload) extends Message

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