package com.thrive.game

import com.badlogic.gdx._
import com.thrive.game.controller.WorldController
import com.thrive.game.render.WorldRenderer
import com.thrive.simulation.world.World
import org.joda.time.DateTime
import org.joda.time.chrono.GJChronology

class ThriveApp extends Game {
  private val TAG = classOf[ThriveApp].getName

  override def create(): Unit = {
    Gdx.app.setLogLevel(Application.LOG_DEBUG)

    val chrono = GJChronology.getInstance

    val startingTime = new DateTime(1066, 10, 14, 10, 0, 0, 0, chrono)
    val world = World.randomWorld(startingTime)

    val worldController = WorldController(world = world, time = startingTime)
    setScreen(WorldRenderer(worldController))
  }
}