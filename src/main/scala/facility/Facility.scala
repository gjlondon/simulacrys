package facility

import java.util.UUID

import actions._
import configuration.Configuration.DEBUG
import entity.Entity
import facility.Handlers.FarmReplyHandlers
import location.Location
import message.MailboxTypes.{Inbox, Outbox}
import message._
import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait Facility extends Entity {
  val name: String = this.getClass.getSimpleName
  val capacity: Int
  val grouping: FacilityGroup
  val isAvailable: Boolean = capacity >= 1
  val replyHandlers: ReplyHandlers

  override def test(m: Specific) = ???
  def reserve: Specific
  def release: Specific
  override def receiveMessages(messages: Queue[Message]): Specific

  def handleReply(reply: Reply,
                  entity: Specific,
                  replyHandlers: ReplyHandlers): Specific = {

    replyHandlers.get(reply.uuid) match {
      case None => entity
      case Some((onSuccess, onFailure)) =>
        if (reply.succeeded) onSuccess(entity) else onFailure(entity)
    }
  }

  def processInbox(inbox: Inbox,
                   entity: Specific,
                   replyHandlers: ReplyHandlers): (Specific, Outbox) = {

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
              val updated = handleReply(rep, entity, replyHandlers)
              go(remaining, updated, outbox)
            // this probably shouldn't happen:
            case _ => (entity, Mailbox.empty)
          }
      }
    }
    val (processedPerson, outbox) = go(inbox, entity, Mailbox.empty)
    // TODO put back inbox emptying
    (processedPerson, outbox)
  }

  private def react(time: DateTime, location: Location, entity: Specific): Specific = {
    // resolve involuntary actions
    // TODO add a concept of thirst

    performNextReaction(datetime = time, location = location,
      entity = entity, reactions = involuntaryActions)
  }

  val involuntaryActions: ReactionCandidates = List()
  type ReactionCandidates = List[(Reaction[Specific], (DateTime, Location, Specific) => Boolean)]

  @tailrec
  private def performNextReaction(datetime: DateTime, location: Location,
                                  entity: Specific,
                                  reactions: ReactionCandidates): Specific = {
    reactions match {
      case Nil => entity
      case (possibleReaction, condition) :: remainingCandidates =>
        val shouldReact = condition(datetime, location, entity)

        val action: Action[Specific] = possibleReaction
        // TODO fix noAction
        //        if (shouldReact) possibleReaction else {
//          val noAction: Action[Specific] = FacilityNoAction()
//          noAction
//        }
        performNextReaction(
          datetime, location,
          action(entity),
          remainingCandidates,
        )
    }
  }

//  override def update(time: DateTime,
//                      location: Location): Specific = {
//
//    val noOpNotReq = Request(
//      from = this.address, to = this.address,
//      payload = NoOp,
//      //      condition = {
//      //        case _: Farm => true
//      //        case _ => false
//      //      },
//      //      onSuccess = (c: Farm) => c,
//      //      onFailure = (c: Farm) => c,
//    )
//
//    val testInbox = inbox.enqueue(noOpNotReq).enqueue(noOpNotReq).enqueue(noOpNotReq)
//
//    // 1. process all messages in inbox, updating state as necessary
//
//    // 2. if appropriate, create some messages to send to entities with whom
//    // you want to interact. These entities may include yourself, who e.g might
//    // complete some action on the next tick that has been started this tick
//
//    // 3. create a set of outgoing replies that respond to any income messages
//    // (to allow senders to react to success or failure), or that initiate an
//    // interaction with another entity
//
//    val (inboxIncorporated, outbox) = processInbox(
//      testInbox,
//      entity = this,
//      replyHandlers = replyHandlers
//    )
//
//
//    val afterReactions: Specific = react(time, location, inboxIncorporated)
//
//    // if entity is not incapacitated, allow a voluntary action
//    // by assumption, no action is allowed to take less than the length of a single tick
//    // so it's safe to assume that in a given tick, a entity will take at most one action
//
//    afterReactions.copy(outbox = outbox)
//  }
}

object Handlers {
  type PastureReplyHandlers = Map[UUID, (Pasture => Pasture, Pasture => Pasture)]

  val emptyPastureReplyHandler: PastureReplyHandlers = {
    Map[UUID, (Pasture => Pasture, Pasture => Pasture)]()
  }

  type FarmReplyHandlers = Map[UUID, (Farm => Farm, Farm => Farm)]

  val emptyFarmReplyHandler: FarmReplyHandlers = {
    Map[UUID, (Farm => Farm, Farm => Farm)]()
  }

  type ForestReplyHandlers = Map[UUID, (Forest => Forest, Forest => Forest)]

  val emptyForestReplyHandler: ForestReplyHandlers = {
    Map[UUID, (Forest => Forest, Forest => Forest)]()
  }
}

import facility.Handlers._

case class Pasture(capacity: Int = 3,
                   inbox: Inbox = Mailbox.empty,
                   outbox: Outbox = Mailbox.empty,
                  )
  extends Facility {
  override type Specific = Pasture
  val grouping: Pastures.type = Pastures


  // override val replyHandlers: ReplyHandlers = emptyPastureReplyHandler
  override val replyHandlers: ReplyHandlers = emptyPastureReplyHandler

  override def reserve: Pasture = ???

  override def release: Pasture = ???

  override def receiveMessages(messages: Queue[Message]): Pasture = ???

  override def update(time: DateTime, location: Location): Pasture = ???

  override def handleRequest(req: Request, Specific: Pasture): (Pasture, Reply) = ???

  override def requestSucceeds(payload: MessagePayload, Specific: Pasture): Boolean = ???

  override def onRequestSuccess(payload: MessagePayload, Specific: Pasture): Pasture = ???

  override def onRequestFailure(payload: MessagePayload, Specific: Pasture): Pasture = ???
}

case class Farm(capacity: Int = 2,
                inbox: Inbox = Mailbox.empty,
                outbox: Outbox = Mailbox.empty,
                replyHandlers: FarmReplyHandlers = emptyFarmReplyHandler
               )
  extends Facility {
  override type Specific = Farm
  val grouping: Farms.type = Farms
  override def reserve: Farm = this.copy(capacity = capacity - 1)

  override def release: Farm = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Farm = {
    this.copy(inbox = inbox ++ messages)
  }

  override def update(time: DateTime,
                      location: Location): Farm = {

    val noOpNotReq = Request(
      from = this.address, to = this.address,
      payload = NoOp,
//      condition = {
//        case _: Farm => true
//        case _ => false
//      },
//      onSuccess = (c: Farm) => c,
//      onFailure = (c: Farm) => c,
    )

    val testInbox = inbox.enqueue(noOpNotReq).enqueue(noOpNotReq).enqueue(noOpNotReq)

    // 1. process all messages in inbox, updating state as necessary

    // 2. if appropriate, create some messages to send to entities with whom
    // you want to interact. These entities may include yourself, who e.g might
    // complete some action on the next tick that has been started this tick

    // 3. create a set of outgoing replies that respond to any income messages
    // (to allow senders to react to success or failure), or that initiate an
    // interaction with another entity

    val (inboxIncorporated, outbox) = processInbox(
      testInbox,
      entity = this,
      replyHandlers = replyHandlers
    )


    val afterReactions: Farm = react(time, location, inboxIncorporated)

    // if entity is not incapacitated, allow a voluntary action
    // by assumption, no action is allowed to take less than the length of a single tick
    // so it's safe to assume that in a given tick, a entity will take at most one action

    afterReactions.copy(outbox = outbox)
  }


  def requestSucceeds(payload: MessagePayload, entity: Farm): Boolean = {
    payload match {
      case NoOp => true
    }
  }

  def onRequestSuccess(payload: MessagePayload, entity: Farm): Farm = {
    payload match {
      case NoOp => entity
    }
  }

  def onRequestFailure(payload: MessagePayload, entity: Farm): Farm = {
    payload match {
      case NoOp => entity
    }
  }

  def handleRequest(req: Request,
                    entity: Farm): (Farm, Reply) = {
    val success = requestSucceeds(req.payload, entity)
    val update: Farm => Farm = if(success)
      onRequestSuccess(payload = req.payload, _)
    else onRequestFailure(payload = req.payload, _)
    val updated: Farm = update(entity)
    val reply = Reply(
      from = updated.address,
      to = req.from,
      succeeded = success,
      re = req.uuid
    )

    (updated, reply)
  }

  override def handleReply(reply: Reply,
                           person: Farm,
                           replyHandlers: FarmReplyHandlers): Farm = {

    replyHandlers.get(reply.uuid) match {
      case None => person
      case Some((onSuccess, onFailure)) =>
        if (reply.succeeded) onSuccess(person) else onFailure(person)
    }
  }

  override def processInbox(inbox: Inbox,
                            entity: Farm,
                            replyHandlers: FarmReplyHandlers): (Farm, Outbox) = {

    @tailrec
    def go(inbox: Inbox, person: Farm, outbox: Outbox): (Farm, Outbox) = {
      inbox.dequeueOption match {
        case None => (person, Mailbox.empty)
        case Some((message, remaining)) =>
          message match {
            case req: Request =>
              val (updated, reply) = handleRequest(req, person)
              go(remaining, updated, outbox.enqueue(reply))
            case rep: Reply =>
              val updated = handleReply(rep, person, replyHandlers)
              go(remaining, updated, outbox)
            // this probably shouldn't happen:
            case _ => (person, Mailbox.empty)
          }
      }
    }
    val (processedPerson, outbox) = go(inbox, entity, Mailbox.empty)
    (processedPerson.copy(inbox = Mailbox.empty), outbox)
  }

  private def react(time: DateTime, location: Location, entity: Farm): Farm = {
    // resolve involuntary actions
    // TODO add a concept of thirst

    performNextReaction(datetime = time, location = location,
      entity = entity, reactions = involuntaryActions)
  }

  override val involuntaryActions: ReactionCandidates = List()
  override type ReactionCandidates = List[(Reaction[Specific], (DateTime, Location, Specific) => Boolean)]

  @tailrec
  private def performNextReaction(datetime: DateTime, location: Location,
                                  entity: Farm,
                                  reactions: ReactionCandidates): Farm = {
    reactions match {
      case Nil => entity
      case (possibleReaction, condition) :: remainingCandidates =>
        val shouldReact = condition(datetime, location, entity)
        if (DEBUG && shouldReact) {
          val msg = s"Person ${entity.name} should perform ${possibleReaction.name} at time $datetime"
          println(msg)
        }
        val action: Action[Farm] = if (shouldReact) possibleReaction else FarmNoAction
        performNextReaction(
          datetime, location,
          action(entity),
          remainingCandidates,
        )
    }
  }

}

case class Forest(capacity: Int = 1, inbox: Inbox = Mailbox.empty,
                  outbox: Outbox = Mailbox.empty,
                  )
  extends Facility {
  override type Specific = Forest
  val grouping: Forests.type = Forests

  override def reserve: Forest = this.copy(capacity = capacity - 1)

  override def release: Forest = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Forest = {
    this.copy(inbox = inbox ++ messages)
  }

  override def update(time: DateTime, location: Location): Forest = ???

  override val replyHandlers: ReplyHandlers = emptyForestReplyHandler

  override def handleRequest(req: Request, Specific: Forest): (Forest, Reply) = ???

  override def requestSucceeds(payload: MessagePayload, Specific: Forest): Boolean = ???

  override def onRequestSuccess(payload: MessagePayload, Specific: Forest): Forest = ???

  override def onRequestFailure(payload: MessagePayload, Specific: Forest): Forest = ???
}


sealed trait FacilityGroup

case object Farms extends FacilityGroup
case object Pastures extends FacilityGroup
case object Forests extends FacilityGroup