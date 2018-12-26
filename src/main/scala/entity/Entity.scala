package entity

import java.util.UUID
import java.util.UUID.randomUUID

import actions.{Action, Reaction}
import location.Location
import message.MailboxTypes.{Inbox, Outbox}
import message._
import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait Entity {
  /** base trait for any "object" in the world that is capable of acting or being acted on
    * e.g. people, animals, chairs.
    *
    * Each will have a mailbox where they receive incoming messages which they will
    * process and respond to before taking an "voluntary" actions
    */
  type Specific <: Entity
  type RelevantAction <: Action
  type ReplyHandlers = Map[UUID, (Specific => Specific, Specific => Specific)]
  val address: UUID = randomUUID()

  val inbox: Queue[Message]
  val outbox: Queue[Message]
  val replyHandlers: ReplyHandlers

  def update(time: DateTime, location: Location): Specific
  def receiveMessages(messages: Queue[Message]): Specific
  def requestSucceeds(payload: MessagePayload, specific: Specific): Boolean
  def onRequestSuccess(payload: MessagePayload, specific: Specific): Specific
  def onRequestFailure(payload: MessagePayload, specific: Specific): Specific
  def initiateAction(action: RelevantAction, entity: Specific): (Specific, Outbox)

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

  def handleInbox(entity: Specific): (Specific, Outbox)

  def consumeInbox(inbox: Inbox,
                   entity: Specific): (Specific, Outbox) = {

    @tailrec
    def go(inbox: Inbox, entity: Specific, outbox: Outbox): (Specific, Outbox) = {
      inbox.dequeueOption match {
        case None => (entity, Mailbox.empty)
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
            case _ => (entity, Mailbox.empty)
          }
      }
    }
    val (processedPerson, outbox) = go(inbox, entity, Mailbox.empty)
    (processedPerson, outbox)
  }

  def react(time: DateTime, location: Location, entity: Specific): Specific = {
    // resolve involuntary actions
    // TODO add a concept of thirst

    performNextReaction(datetime = time, location = location,
      entity = entity, reactions = involuntaryActions)
  }

  val involuntaryActions: ReactionCandidates = List()
  type ReactionCandidates = List[(RelevantAction, (DateTime, Location, Specific) => Boolean)]

  def performNextReaction(datetime: DateTime, location: Location,
                                  entity: Specific,
                                  reactions: ReactionCandidates): Specific = {
    reactions match {
      case Nil => entity
      case (possibleReaction, condition) :: remainingCandidates =>
        val shouldReact = condition(datetime, location, entity)

        val action: RelevantAction = possibleReaction
        // TODO fix noAction
//                if (shouldReact) possibleReaction else {
//                  val noAction: Action = FacilityNoAction()
//                  noAction
//                }
        // TODO handle outbox stemming from reactions
        val (updated, outbox) = initiateAction(action, entity)
        performNextReaction(
          datetime, location,
          updated,
          remainingCandidates,
        )
    }
  }
}
