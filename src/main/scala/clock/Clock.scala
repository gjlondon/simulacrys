package clock

import world.World

import scala.annotation.tailrec

object Clock {

  @tailrec
  def tick(tickNum: Int = 0, maxTicks: Int, world: World): World = {
    if (tickNum >= maxTicks) return world


    val newLocations = world.grid.positions.par.map { loc =>
      val updatedPopulace = loc.populace map { p => p.act() }
      loc.withNewPopulace(populace = updatedPopulace)
    }

    val newWorld = World.fromLocations(newLocations.toVector)
    printTick(tickNum, newWorld = newWorld)

    tick(tickNum + 1, maxTicks, newWorld)
  }

  private def printTick(tickNum: Int, newWorld: World): Unit = {
    if (tickNum % 10 == 0) {
      println(s"Tick number $tickNum")
      newWorld.printSummary()
    }
  }
}
