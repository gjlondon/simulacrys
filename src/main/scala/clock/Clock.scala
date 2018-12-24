package clock

import com.github.nscala_time.time.Imports._
import configuration.Configuration
import constants.Constants.TICK_DURATION
import location.Location
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import world.World

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object Clock {
  val dtFMT: DateTimeFormatter = DateTimeFormat.mediumDateTime()
  var popSeries = new ListBuffer[Int]

  @tailrec
  def tick(tickNum: Int = 0, maxTicks: Int, world: World, time: DateTime): World = {
    if (tickNum >= maxTicks) return world

    // TODO replace with some kind of partial (was getting collection construction errors when I tried)

    val newLocations =
      if (Configuration.PARALLEL) {
        world.grid.positions.par.map(updateLocation(time, world))
      }
      else {
        world.grid.positions.map(updateLocation(time, world))
      }

    val newWorld = World.fromLocations(newLocations.toVector)
    if (Configuration.DEBUG) debugPopulationGrowth(newWorld)

    printTick(tickNum, newWorld = newWorld, time = time, maxTicks = maxTicks)

    tick(tickNum + 1, maxTicks, newWorld, time = time + TICK_DURATION)
  }

  def updateLocation(time: DateTime, world: World)(loc: Location): Location = {
    val updatedPopulace = loc.populace map { p => p.update(time = time, world = world) }
    loc.withNewPopulace(populace = updatedPopulace.living)
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
