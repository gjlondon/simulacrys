package entity

import java.util.UUID
import java.util.UUID.randomUUID

import location.Location
import message._
import org.joda.time.DateTime

import scala.collection.immutable.Queue

trait Entity {
  /** base trait for any "object" in the world that is capable of acting or being acted on
    * e.g. people, animals, chairs.
    *
    * Each will have a mailbox where they receive incoming messages which they will
    * process and respond to before taking an "voluntary" actions
    */
  type Specific <: Entity
  type ReplyHandlers = Map[UUID, (Specific => Specific, Specific => Specific)]
  val address: UUID = randomUUID()

  val inbox: Queue[Message]
  val outbox: Queue[Message]

  def test(m: Specific): Unit
  def update(time: DateTime, location: Location): Specific
  def receiveMessages(messages: Queue[Message]): Specific
  def handleRequest(req: Request,
                    Specific: Specific): (Specific, Reply)


  def requestSucceeds(payload: MessagePayload, Specific: Specific): Boolean

  def onRequestSuccess(payload: MessagePayload, Specific: Specific): Specific

  def onRequestFailure(payload: MessagePayload, Specific: Specific): Specific



}
