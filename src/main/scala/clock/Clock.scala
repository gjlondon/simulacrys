package clock

import person.Person
import status.Dead
import world.World
import populace.Populace

object Clock {

  def tick(tickNum: Int = 0, maxTicks: Int, world: World): Unit = {
    if (tickNum >= maxTicks) return

    println(s"Tick number $tickNum")

    val locations = for {
      location <- world.grid.positions
      updatedPeople <- location.populace map { p => p.act() }
    } yield location.withNewPopulace(populace = Populace(updatedPeople))

    val newWorld = World.fromLocations(locations)
//  world.grid map { location =>
//      location.populace map  fullPopulace take 10 foreach { person =>
//      println(person)
//    }

    tick(tickNum + 1, maxTicks, newWorld)



//    var livingPopulace = world.fullPopulace
//    livingPopulace = livingPopulace.filterNot(person => person.health == Dead)
//    livingPopulace = livingPopulace.map { p =>
//      p.act()
//
//      println(s"Round $tickNum:")
//      printStatus(livingPopulace)
//
//    }
  }

  def printStatus(populace: Set[Person]): Unit = {
    for {
      person <- populace
    } {
      // println(s"${person.name} in ${person.location.name} is ${person.health}")

    }
  }
}
