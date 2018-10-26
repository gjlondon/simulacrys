import demographic.{Adult, Female, Male}
import inventory.Inventory
import location.City
import person.Commoner
import resource.{Fat, Protein}

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

    var populace = Set(alice, bob, carl)


    0 to 10 foreach { tick =>
      populace = populace.map { p =>
        p.act()
      }
      println(populace)
    }

  }
}