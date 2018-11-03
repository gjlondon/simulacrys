import clock.Clock
import person.Person
import world.World


object Main extends App {
  override def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val world = World.randomWorld

    Clock.tick(maxTicks = 3, world = world)

  }

  def printStatus(populace: Set[Person]): Unit = {
    for {
      person <- populace
    } {
//      println(s"${person.name} in ${person.location.name} is ${person.health}")

    }
  }
}