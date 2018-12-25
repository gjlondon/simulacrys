package message

import java.util.UUID
import java.util.UUID.randomUUID

import scala.collection.immutable.Queue
import scala.reflect.ClassTag


sealed trait Message {
  val uuid: UUID = randomUUID()
}

case class Request[+T, U](from: T, to: U,
                          condition: U => Boolean,
                          onSuccess: U => U,
                          onFailure: U => U) extends Message

case class Reply[T: ClassTag, U: ClassTag](re: UUID,
                                           from: T,
                                           to: U,
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
}