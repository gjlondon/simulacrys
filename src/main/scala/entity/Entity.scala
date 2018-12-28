package entity

import java.util.UUID

import actions.Action
import location.Location
import message.MailboxTypes.{Inbox, Outbox}
import message._
import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** base trait for any "object" in the world that is capable of acting or being acted on
  * e.g. people, animals, chairs.
  *
  * Each will have a mailbox where they receive incoming messages which they will
  * process and respond to before taking an "voluntary" actions
  */
trait Entity {

  type Specific <: Entity
  type RelevantAction <: Action
  type ReplyHandlers = Map[UUID, (Specific => Specific, Specific => Specific)]
  type ReactionCandidates = List[(RelevantAction, (DateTime, Location, Specific) => Boolean)]

  val NoAction: RelevantAction
  val involuntaryActions: ReactionCandidates = List()
  val address: UUID
  val inbox: Queue[Message]
  val outbox: Queue[Message]
  val replyHandlers: ReplyHandlers

  def update(time: DateTime, location: Location): Specific
  def receiveMessages(messages: Queue[Message]): Specific
  def requestSucceeds(payload: MessagePayload, specific: Specific): Boolean
  def onRequestSuccess(payload: MessagePayload, specific: Specific): Specific
  def onRequestFailure(payload: MessagePayload, specific: Specific): Specific
  def initiateAction(action: RelevantAction, entity: Specific): (Specific, Outbox)
  def handleInbox(entity: Specific): (Specific, Outbox)

  def handleRequest(req: Request,
                    entity: Specific): (Specific, Reply) = {
    val success = requestSucceeds(req.payload, entity)
    val update: Specific => Specific = if(success)
      onRequestSuccess(payload = req.payload, _)
    else onRequestFailure(payload = req.payload, _)
    val updated: Specific = update(entity)
    val reply = Reply(
      from = updated.address,
      to = req.from,
      succeeded = success,
      re = req.uuid
    )

    (updated, reply)
  }

  def handleReply(reply: Reply,
                  entity: Specific): Specific = {
    replyHandlers.get(reply.uuid) match {
      case None => entity
      case Some((onSuccess, onFailure)) =>
        if (reply.succeeded) onSuccess(entity) else onFailure(entity)
    }
  }

  def consumeInbox(inbox: Inbox,
                   entity: Specific): (Specific, Outbox) = {

    @tailrec
    def go(inbox: Inbox, entity: Specific, outbox: Outbox): (Specific, Outbox) = {
      inbox.dequeueOption match {
        case None => (entity, outbox)
        case Some((message, remaining)) =>
          message match {
            case req: Request =>
              // safe to coerce because we've just checked the type compliance
              val (updated, reply) = handleRequest(req, entity)
              go(remaining, updated, outbox.enqueue(reply))
            case rep: Reply =>
              val updated = handleReply(rep, entity)
              go(remaining, updated, outbox)
            // this probably shouldn't happen:
            case _ => (entity, outbox)
          }
      }
    }
    val (processedPerson, outbox) = go(inbox, entity, Mailbox.empty)
    (processedPerson, outbox)
  }

  def react(time: DateTime, location: Location, entity: Specific): (Specific, Outbox) = {
    // resolve involuntary actions
    // TODO add a concept of thirst

    performNextReaction(datetime = time, location = location,
      entity = entity, reactions = involuntaryActions)
  }

  def performNextReaction(datetime: DateTime, location: Location,
                                  entity: Specific,
                                  reactions: ReactionCandidates): (Specific, Outbox) = {

    @tailrec
    def go(entity: Specific,
           reactions: ReactionCandidates,
           acc: Outbox): (Specific, Outbox) = {
      reactions match {
        case Nil => (entity, Mailbox.empty)
        case (possibleReaction, condition) :: remainingCandidates =>
          val shouldReact = condition(datetime, location, entity)
          val action: RelevantAction = if (shouldReact) possibleReaction else NoAction
          val (updated, outbox) = initiateAction(action, entity)
          go(
            entity = updated,
            reactions = remainingCandidates,
            acc = acc ++ outbox
          )
      }

    }
    val (processedPerson, outbox) = go(entity, reactions, Mailbox.empty)
    (processedPerson, outbox)
  }
}
