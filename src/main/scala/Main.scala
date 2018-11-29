import clock.Clock
import org.joda.time.DateTime
import org.joda.time.chrono.GJChronology
import world.World



object Main extends App {
  import configuration.Configuration.MAX_TICKS
  val chrono = GJChronology.getInstance

  override def main(args: Array[String]): Unit = {
    val world = World.randomWorld

    val startingTime = new DateTime(1066, 10, 14, 10, 0, 0, 0, chrono)

    val finalWorld = Clock.tick(maxTicks = MAX_TICKS, world = world, time=startingTime)
    finalWorld.printOverview()
  }
}