package clock

import world.World

import scala.annotation.tailrec

object Clock {

  @tailrec
  def tick(tickNum: Int = 0, maxTicks: Int, world: World): World = {
    if (tickNum >= maxTicks) return world

    println(s"Tick number $tickNum")

    val newLocations = world.grid.positions map { loc =>
      val updatedPopulace = loc.populace map { p => p.act() }
      loc.withNewPopulace(populace = updatedPopulace)
    }

    val newWorld = World.fromLocations(newLocations)

    tick(tickNum + 1, maxTicks, newWorld)
  }
}
