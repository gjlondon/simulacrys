package clock

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import world.World

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object Clock {
  val PARALLEL = false
  val dtFMT: DateTimeFormatter = DateTimeFormat.mediumDateTime()
  var popSeries = new ListBuffer[Int]

  @tailrec
  def tick(tickNum: Int = 0, maxTicks: Int, world: World, time: DateTime): World = {
    if (tickNum >= maxTicks) return world

    // TODO replace with some kind of partial (was getting collection construction errors when I tried)
    val newLocations =
      if (PARALLEL) {
        world.grid.positions.par.map { loc =>
          val updatedPopulace = loc.populace map { p => p.act(time = time, world = world) }
          loc.withNewPopulace(populace = updatedPopulace)
        }
      }
      else {
        world.grid.positions.map { loc =>
          val updatedPopulace = loc.populace map { p => p.act(time = time, world = world) }
          loc.withNewPopulace(populace = updatedPopulace)
        }
      }

    val newWorld = World.fromLocations(newLocations.toVector)
    debugPopulationGrowth(newWorld)

    printTick(tickNum, newWorld = newWorld, time = time, maxTicks = maxTicks)

    tick(tickNum + 1, maxTicks, newWorld, time = time + 1.hours)
  }

  private def debugPopulationGrowth(newWorld: World) = {
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
