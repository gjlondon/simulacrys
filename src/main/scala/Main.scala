import clock.Clock
import world.World


object Main extends App {
  override def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val world = World.randomWorld

    Clock.tick(maxTicks = 3, world = world)

  }
}