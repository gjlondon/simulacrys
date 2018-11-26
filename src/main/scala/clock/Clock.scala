package clock

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import world.World

import scala.annotation.tailrec

object Clock {
  val PARALLEL = false
  val dtFMT: DateTimeFormatter = DateTimeFormat.mediumDateTime()

  @tailrec
  def tick(tickNum: Int = 0, maxTicks: Int, world: World, time: DateTime): World = {
    if (tickNum >= maxTicks) return world


    // TODO replace with some kind of partial (was getting collection construction errors when I tried)
    val newLocations =
      if (PARALLEL) {
        world.grid.positions.par.map { loc =>
          val updatedPopulace = loc.populace map { p => p.act(time = time) }
          loc.withNewPopulace(populace = updatedPopulace)
        }
      }
      else {
        world.grid.positions.map { loc =>
          val updatedPopulace = loc.populace map { p => p.act(time = time) }
          loc.withNewPopulace(populace = updatedPopulace)
        }
      }

    val newWorld = World.fromLocations(newLocations.toVector)
    printTick(tickNum, newWorld = newWorld, time = time)

    tick(tickNum + 1, maxTicks, newWorld, time = time + 1.hours)
  }

  private def printTick(tickNum: Int, newWorld: World, time: DateTime): Unit = {
    if (tickNum % 10 == 0) {
      println(s"Tick number $tickNum at ${dtFMT.print(time)}")
      newWorld.printSummary()
    }
  }
}
