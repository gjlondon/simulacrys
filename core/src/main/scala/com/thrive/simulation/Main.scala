package com.thrive.simulation

import org.joda.time.DateTime
import org.joda.time.chrono.GJChronology
import com.thrive.simulation.clock.Clock
import com.thrive.simulation.world.World

object Main extends App {
  import configuration.Configuration.MAX_TICKS
  val chrono = GJChronology.getInstance

  override def main(args: Array[String]): Unit = {

    val startingTime = new DateTime(1066, 10, 14, 10, 0, 0, 0, chrono)
    val world = World.randomWorld(startingTime)

    val finalWorld = Clock.tick(maxTicks = MAX_TICKS, world = world, time=startingTime)
    finalWorld.printOverview()
  }
}
