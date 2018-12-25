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

  def reserve: Specific
  def release: Specific
  override def receiveMessages(messages: Queue[Message]): Specific


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

  override val involuntaryActions: ReactionCandidates = List()
  override type ReactionCandidates = List[(Reaction[Specific], (DateTime, Location, Specific) => Boolean)]


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