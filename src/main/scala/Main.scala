import demographic.{Adult, Female, Male}
import inventory.Inventory
import location.City
import person.{Commoner, Person}
import resource.{Fat, Protein}
import status.Dead

object Main extends App {
  override def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val waterdeep = City(name = "Waterdeep", latitude = 5, longitude = 5)
    val bob = Commoner("Bob", Inventory.fromManifest(Map(Fat -> 30, Protein -> 20)),
      location = waterdeep, age = Adult, gender = Male)
    val carl = Commoner("Carl", Inventory.fromManifest(Map(Fat -> 20, Protein -> 10)),
      location = waterdeep, age = Adult, gender = Male)
    val alice = Commoner("Alice", Inventory.fromManifest(Map(Fat -> 10, Protein -> 5)),
      location = waterdeep, age = Adult, gender = Female)

    val startingPopulace: Set[Person] = Set(alice, bob, carl)
    var livingPopulace = startingPopulace

    0 to 20 foreach { tick =>
      livingPopulace = livingPopulace.filterNot(person => person.health == Dead)
      livingPopulace = livingPopulace.map { p =>
        p.act()
      }
      println(s"Round $tick:")
      printStatus(livingPopulace)
    }
  }

  def printStatus(populace: Set[Person]): Unit = {
    for {
      person <- populace
    } {
      println(s"${person.name} in ${person.location.name} is ${person.health}")

    }
  }
}