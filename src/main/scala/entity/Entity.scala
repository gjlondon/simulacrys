package entity

import java.util.UUID
import java.util.UUID.randomUUID

import message.Message

import scala.collection.immutable.Queue

trait Entity {
  /** base trait for any "object" in the world that is capable of acting or being acted on
    * e.g. people, animals, chairs.
    *
    * Each will have a mailbox where they receive incoming messages which they will
    * process and respond to before taking an "voluntary" actions
    */
  val address: UUID = randomUUID()

  val inbox: Queue[Message]
  val outbox: Queue[Message]
}
