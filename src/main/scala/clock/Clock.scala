package clock

import world.World

object Clock {

  def tick(tickNum: Int = 0, maxTicks: Int, world: World): Unit = {
    if (tickNum >= maxTicks) return

    println(s"Tick number $tickNum")

    val newLocations = world.grid.positions map { loc =>
      val updatedPeople = loc.populace map { p => p.act() }
      loc.withNewPopulace(populace = updatedPeople)
    }

    val newWorld = World.fromLocations(newLocations)

    newWorld.printOverview()

    tick(tickNum + 1, maxTicks, newWorld)
  }
}
