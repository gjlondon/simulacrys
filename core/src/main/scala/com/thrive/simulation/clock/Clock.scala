package com.thrive.simulation.clock

import java.util.UUID

import com.github.nscala_time.time.Imports._
import com.thrive.simulation.configuration.Configuration
import com.thrive.simulation.constants.Constants.TICK_DURATION
import com.thrive.simulation.entity.Entity
import com.thrive.simulation.location.Location
import com.thrive.simulation.message.MailboxTypes.Mailbox
import com.thrive.simulation.message.{Mailbox, Message}
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import com.thrive.simulation.world.World

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer


object Clock {
  val dtFMT: DateTimeFormatter = DateTimeFormat.mediumDateTime()
  var popSeries = new ListBuffer[Int]

  @tailrec
  def recursiveTick(tickNum: Int = 0, maxTicks: Int, world: World, time: DateTime): (World, DateTime) = {
    if (tickNum >= maxTicks) return (world, time)

    // TODO replace with some kind of partial (was getting collection construction errors when I tried)

    val results =
      if (Configuration.PARALLEL) {
        world.grid.positions.par.map(updateLocation(time, world))
      }
      else {
        world.grid.positions.map(updateLocation(time, world))
      }

    val (newLocations, outgoingMessages) = results.unzip
    // TODO: this seems like a big performance hit -- can it be faster?
    val allOutgoingMessages = outgoingMessages.foldLeft(Mailbox.empty)((a, b) => a ++ b)
    val locationsPostDelivery = deliverMessagesToAllLocations(
      newLocations.toList,
      allOutgoingMessages
    )
    val newWorld = World.fromLocations(locationsPostDelivery.toVector)
    if (Configuration.DEBUG) debugPopulationGrowth(newWorld)

//    printTick(tickNum, newWorld = newWorld, time = time, maxTicks = maxTicks)

    recursiveTick(tickNum + 1, maxTicks, newWorld, time = time + TICK_DURATION)
  }

  def deliverMessagesToAllLocations(locations: List[Location],
                                    messages: Mailbox): List[Location] = {
    val messagesByRecipient = messages.groupBy { msg: Message => msg.to }
    locations.par.map { l: Location =>
      deliverMessagesToLocation(l, messagesByRecipient)
    }.toList
  }

  def tick(world: World, time: DateTime): (World, DateTime) = {
    // TODO replace with some kind of partial (was getting collection construction errors when I tried)

    val results =
      if (Configuration.PARALLEL) {
        world.grid.positions.par.map(updateLocation(time, world))
      }
      else {
        world.grid.positions.map(updateLocation(time, world))
      }

    val (newLocations, outgoingMessages) = results.unzip
    // TODO: this seems like a big performance hit -- can it be faster?
    val allOutgoingMessages = outgoingMessages.foldLeft(Mailbox.empty)((a, b) => a ++ b)
    val locationsPostDelivery = deliverMessagesToAllLocations(
      newLocations.toList,
      allOutgoingMessages
    )
    val newWorld = World.fromLocations(locationsPostDelivery.toVector)
    if (Configuration.DEBUG) debugPopulationGrowth(newWorld)

    (newWorld, time + TICK_DURATION)
  }

  def deliverMessagesToLocation(location: Location,
                                messagesByRecipient: Map[UUID, Queue[Message]]): Location = {
    val delivered: Set[Entity] = location.entities.par.map { e: Entity =>
      val messagesToEntity = messagesByRecipient.getOrElse(
        e.address, Queue[Message]()
      )
      val received: Entity = e.receiveMessages(messagesToEntity)
      received
    }.seq
    location.withNewEntities(delivered)
  }


  def updateLocation(time: DateTime, world: World)(loc: Location): (Location, Mailbox) = {
    val updatedEntities = loc.entities map { p =>
      val updated: Entity = p.update(time = time, location = loc)
      updated
    }
    (loc.withNewEntities(entities = updatedEntities), outgoingMessages(updatedEntities))
  }

  def outgoingMessages(entities: Set[Entity]): Mailbox = {
    entities.foldLeft(Mailbox.empty)((soFar, entity) => soFar ++ entity.outbox)
  }

  private def debugPopulationGrowth(newWorld: World): Unit = {
    val totalPopulation = newWorld.totalPopulation
    popSeries += totalPopulation
    val toCompare = 1000
    val lastFew = popSeries.takeRight(toCompare)
    if (lastFew.size == toCompare && lastFew.forall(_ == lastFew.head)) {
      println("Population unchanged")
    }
  }

  private def printTick(tickNum: Int, newWorld: World, time: DateTime, maxTicks: Int): Unit = {
    if (tickNum % (maxTicks / 100) == 0) {
      println(s"Tick number $tickNum at ${dtFMT.print(time)}")
      newWorld.printSummary()
    }
  }
}
