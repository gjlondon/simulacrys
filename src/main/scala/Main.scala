import clock.Clock
import world.World


object Main extends App {
  override def main(args: Array[String]): Unit = {
    val world = World.randomWorld
    val finalWorld = Clock.tick(maxTicks = 300, world = world)
    finalWorld.printOverview()
  }
}