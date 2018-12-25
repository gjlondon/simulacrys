package clock

import java.util.UUID

import com.github.nscala_time.time.Imports._
import configuration.Configuration
import constants.Constants.TICK_DURATION
import facility.Facility
import location.Location
import message.MailboxTypes.Mailbox
import message.{Mailbox, Message}
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import person.Person
import populace.Populace
import world.World

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer


object Clock {
  val dtFMT: DateTimeFormatter = DateTimeFormat.mediumDateTime()
  var popSeries = new ListBuffer[Int]

  @tailrec
  def tick(tickNum: Int = 0, maxTicks: Int, world: World, time: DateTime): World = {
    if (tickNum >= maxTicks) return world

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

    printTick(tickNum, newWorld = newWorld, time = time, maxTicks = maxTicks)

    tick(tickNum + 1, maxTicks, newWorld, time = time + TICK_DURATION)
  }

  def deliverMessagesToAllLocations(locations: List[Location],
                                    messages: Mailbox): List[Location] = {
    val messagesByRecipient = messages.groupBy { msg: Message => msg.to }
    locations.par.map { l: Location =>
      deliverMessagesToLocation(l, messagesByRecipient)

    }.toList
  }

  def deliverMessagesToLocation(location: Location,
                                messagesByRecipient: Map[UUID, Queue[Message]]): Location = {
    val deliveredPeople = location.populace.par.map { p: Person =>
      val messagesToPerson = messagesByRecipient.getOrElse(p.address, Queue[Message]())
      p.receiveMessages(messagesToPerson)
    }.seq.toSeq
//    val deliveredFacilities = location.facilities.par.map { f: Facility =>
//      val messagesToFacility = messagesByRecipient.getOrElse(f.address, Queue[Message]())
//      f.receiveMessages(messagesToFacility)
//    }.seq.toSeq
    location.withNewPopulace(Populace(deliveredPeople: _*))
  }


  def updateLocation(time: DateTime, world: World)(loc: Location): (Location, Mailbox) = {
    val updatedPopulace = loc.populace map { p => p.update(time = time, location = loc) }
    val outgoingMessages = updatedPopulace.outgoingMessages
    (loc.withNewPopulace(populace = updatedPopulace.living), outgoingMessages)
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
