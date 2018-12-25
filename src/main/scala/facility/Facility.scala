package facility

import java.util.UUID

import actions.{Action, FarmNoAction}
import configuration.Configuration.DEBUG
import entity.Entity
import facility.Handlers.PastureReplyHandlers
import location.Location
import message.MailboxTypes.{Inbox, Outbox}
import message.{Mailbox, Message, Reply, Request}
import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait Facility extends Entity {
  val name: String = this.getClass.getSimpleName
  val capacity: Int
  val grouping: FacilityGroup
  val isAvailable: Boolean = capacity >= 1

  def reserve: Facility
  def release: Facility
  def receiveMessages(messages: Queue[Message]): Facility

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
                   replyHandlers: PastureReplyHandlers = emptyPastureReplyHandler
                  )
  extends Facility {
  val grouping: Pastures.type = Pastures
  override def reserve: Pasture = this.copy(capacity = capacity - 1)

  override def release: Pasture = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Pasture = {
    this.copy(inbox = inbox ++ messages)
  }

  override def update(time: DateTime, location: Location): Entity = ???

  override def handleRequest(req: Request[Entity], entity: Entity): (Entity, Reply) = ???


}

case class Farm(capacity: Int = 2,
                inbox: Inbox = Mailbox.empty,
                outbox: Outbox = Mailbox.empty,
                replyHandlers: FarmReplyHandlers = emptyFarmReplyHandler
               )
  extends Facility {
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

  def handleRequest(req: Request[Farm],
                    person: Farm): (Farm, Reply) = {
    val success = req.condition(person)
    val update = if(success) req.onSuccess else req.onFailure
    val updated: Farm = update(person)
    val reply = Reply(
      from = updated.address,
      to = req.from,
      succeeded = success,
      re = req.uuid
    )

    (updated, reply)
  }

  def handleReply(reply: Reply,
                  person: Farm,
                  replyHandlers: FarmReplyHandlers): Farm = {

    replyHandlers.get(reply.uuid) match {
      case None => person
      case Some((onSuccess, onFailure)) =>
        if (reply.succeeded) onSuccess(person) else onFailure(person)
    }
  }

  def processInbox(inbox: Inbox,
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

  val involuntaryActions: ReactionCandidates = List()

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

  override def handleRequest(req: Request[Entity], entity: Entity): (Entity, Reply) = ???
}

case class Forest(capacity: Int = 1, inbox: Inbox = Mailbox.empty,
                  outbox: Outbox = Mailbox.empty,
                  replyHandlers: ForestReplyHandlers = emptyForestReplyHandler)
  extends Facility {
  val grouping: Forests.type = Forests

  override def reserve: Forest = this.copy(capacity = capacity - 1)

  override def release: Forest = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Forest = {
    this.copy(inbox = inbox ++ messages)
  }

  override def update(time: DateTime, location: Location): Entity = ???

  override def handleRequest(req: Request[Entity], entity: Entity): (Entity, Reply) = ???

}

sealed trait FacilityGroup

case object Farms extends FacilityGroup
case object Pastures extends FacilityGroup
case object Forests extends FacilityGroup