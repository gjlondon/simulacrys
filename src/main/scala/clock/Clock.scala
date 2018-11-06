package clock

import world.World

object Clock {

  def tick(tickNum: Int = 0, world: World): World = {
    println(s"Tick number $tickNum")

    val newLocations = world.grid.positions map { loc =>
      val updatedPopulace = loc.populace map { p => p.act() }
      loc.withNewPopulace(populace = updatedPopulace)
    }

    World.fromLocations(newLocations)
  }
}
