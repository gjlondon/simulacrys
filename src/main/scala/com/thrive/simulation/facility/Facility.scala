package com.thrive.simulation.facility

import java.util.UUID
import java.util.UUID.randomUUID

import com.thrive.simulation.actions._
import com.thrive.simulation.entity.Entity
import com.thrive.simulation.facility.Handlers.FarmReplyHandlers
import com.thrive.simulation.location.Location
import com.thrive.simulation.message.MailboxTypes.{Inbox, Outbox}
import com.thrive.simulation.message._
import org.joda.time.DateTime

import scala.collection.immutable.Queue

sealed trait Facility extends Entity {
  val name: String = this.getClass.getSimpleName
  val capacity: Int
  val maxCapacity: Int
  val grouping: FacilityGroup
  val isAvailable: Boolean = capacity >= 1
  val replyHandlers: ReplyHandlers

  def reserve: Specific
  def release: Specific
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

import com.thrive.simulation.facility.Handlers._

case class Pasture(capacity: Int = 3,
                   inbox: Inbox = Mailbox.empty,
                   outbox: Outbox = Mailbox.empty,
                   address: UUID = randomUUID()
                  )
  extends Facility {
  override type Specific = Pasture
  override type RelevantAction = PastureAction
  val grouping: Pastures.type = Pastures

  override val replyHandlers: ReplyHandlers = emptyPastureReplyHandler

  override def reserve: Pasture = this.copy(capacity = capacity - 1)

  override def release: Pasture = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Pasture = {
    this.copy(inbox = inbox ++ messages)
  }

  override def handleInbox(entity: Pasture): (Pasture, Outbox) = {
    val (updatedEntity, outbox) = consumeInbox(inbox = entity.inbox, entity = entity)
    (updatedEntity.copy(inbox = Mailbox.empty), outbox)
  }

  override def update(time: DateTime, location: Location): Pasture = {

    // 1. process all messages in inbox, updating state as necessary

    // 2. if appropriate, create some messages to send to entities with whom
    // you want to interact. These entities may include yourself, who e.g might
    // complete some action on the next tick that has been started this tick

    // 3. create a set of outgoing replies that respond to any income messages
    // (to allow senders to react to success or failure), or that initiate an
    // interaction with another com.thrive.simulation.entity

    val (inboxIncorporated, outbox) = handleInbox(entity = this)

    val (afterReactions, reactedOutbox) = react(time, location, inboxIncorporated)

    afterReactions.copy(outbox = reactedOutbox)
  }

  def requestSucceeds(payload: MessagePayload, entity: Pasture): Boolean = {
    payload match {
      case NoOp => true
      case Reserve => entity.capacity > 0
      case Release => entity.capacity < entity.maxCapacity
    }
  }

  def onRequestSuccess(payload: MessagePayload, entity: Pasture): Pasture = {
    payload match {
      case NoOp => entity
      case Reserve => reserve
      case Release => release
    }
  }

  def onRequestFailure(payload: MessagePayload, entity: Pasture): Pasture = {
    payload match {
      case NoOp | Reserve | Release => entity
    }
  }

  override def initiateAction(action: PastureAction, entity: Pasture): (Pasture, Outbox) = (this, Mailbox.empty)

  override val NoAction: PastureAction = PastureNoAction
  override val maxCapacity: Int = 3
}

case class Farm(capacity: Int = 2,
                inbox: Inbox = Mailbox.empty,
                outbox: Outbox = Mailbox.empty,
                address: UUID = randomUUID()
               )
  extends Facility {
  override type Specific = Farm
  override type RelevantAction = FarmAction
  val replyHandlers: FarmReplyHandlers = emptyFarmReplyHandler
  val grouping: Farms.type = Farms
  override def reserve: Farm = this.copy(capacity = capacity - 1)

  override def release: Farm = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Farm = {

    this.copy(inbox = inbox ++ messages)
  }

  override def handleInbox(entity: Farm): (Farm, Outbox) = {
    val (updatedEntity, outbox) = consumeInbox(inbox = entity.inbox, entity = entity)
    val inboxCleared = updatedEntity.copy(inbox = Mailbox.empty)
    (inboxCleared, outbox)
  }

  override def update(time: DateTime,
                      location: Location): Farm = {

    // 1. process all messages in inbox, updating state as necessary

    // 2. if appropriate, create some messages to send to entities with whom
    // you want to interact. These entities may include yourself, who e.g might
    // complete some action on the next tick that has been started this tick

    // 3. create a set of outgoing replies that respond to any income messages
    // (to allow senders to react to success or failure), or that initiate an
    // interaction with another com.thrive.simulation.entity
    val (inboxIncorporated, outbox) = handleInbox(entity = this)

    val (afterReactions, reactedOutbox) = react(time, location, inboxIncorporated)

    afterReactions.copy(outbox = outbox ++ reactedOutbox)
  }


  def requestSucceeds(payload: MessagePayload, entity: Farm): Boolean = {
    payload match {
      case NoOp => true
      case Reserve => entity.capacity > 0
      case Release => entity.capacity < entity.maxCapacity
    }
  }

  def onRequestSuccess(payload: MessagePayload, entity: Farm): Farm = {
    payload match {
      case NoOp => entity
      case Reserve => reserve
      case Release => release
    }
  }

  def onRequestFailure(payload: MessagePayload, entity: Farm): Farm = {
    payload match {
      case NoOp | Reserve | Release => entity
    }
  }

  override def initiateAction(action: FarmAction, entity: Farm): (Farm, Outbox) = (this, Mailbox.empty)

  override val NoAction: FarmAction = FarmNoAction
  override val maxCapacity: Int = 2
}

case class Forest(capacity: Int = 1, inbox: Inbox = Mailbox.empty,
                  outbox: Outbox = Mailbox.empty,
                  address: UUID = randomUUID()
                 )
  extends Facility {
  override type Specific = Forest
  val grouping: Forests.type = Forests
  override type RelevantAction = ForestAction

  override def reserve: Forest = this.copy(capacity = capacity - 1)

  override def release: Forest = this.copy(capacity = capacity + 1)

  override def receiveMessages(messages: Queue[Message]): Forest = {
    this.copy(inbox = inbox ++ messages)
  }

  override def handleInbox(entity: Forest): (Forest, Outbox) = {
    val (updatedEntity, outbox) = consumeInbox(inbox = entity.inbox, entity = entity)
    (updatedEntity.copy(inbox = Mailbox.empty), outbox)
  }

  override def update(time: DateTime, location: Location): Forest = {
    // 1. process all messages in inbox, updating state as necessary

    // 2. if appropriate, create some messages to send to entities with whom
    // you want to interact. These entities may include yourself, who e.g might
    // complete some action on the next tick that has been started this tick

    // 3. create a set of outgoing replies that respond to any income messages
    // (to allow senders to react to success or failure), or that initiate an
    // interaction with another com.thrive.simulation.entity

    val (inboxIncorporated, outbox) = handleInbox(entity = this)

    val (afterReactions, reactedOutbox) = react(time, location, inboxIncorporated)

    afterReactions.copy(outbox = reactedOutbox)
  }

  override val replyHandlers: ReplyHandlers = emptyForestReplyHandler

  def requestSucceeds(payload: MessagePayload, entity: Forest): Boolean = {
    payload match {
      case NoOp => true
      case Reserve => entity.capacity > 0
      case Release => entity.capacity < entity.maxCapacity
    }
  }

  def onRequestSuccess(payload: MessagePayload, entity: Forest): Forest = {
    payload match {
      case NoOp => entity
      case Reserve => reserve
      case Release => release
    }
  }

  def onRequestFailure(payload: MessagePayload, entity: Forest): Forest = {
    payload match {
      case NoOp | Reserve | Release => entity
    }
  }

  override def initiateAction(action: ForestAction, entity: Forest): (Forest, Outbox) = (this, Mailbox.empty)

  override val NoAction: ForestAction = ForestNoAction
  override val maxCapacity: Int = 1
}


sealed trait FacilityGroup

case object Farms extends FacilityGroup
case object Pastures extends FacilityGroup
case object Forests extends FacilityGroup