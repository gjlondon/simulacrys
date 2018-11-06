import clock.Clock
import world.World


object Main extends App {
  override def main(args: Array[String]): Unit = {
    var world = World.randomWorld
    var tick = 0
    val maxTicks = 300
    while (tick < maxTicks) {
      world = Clock.tick(tickNum = tick, world = world)
      tick += 1
    }
    world.printOverview()
  }
}