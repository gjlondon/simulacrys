import person.Person
import populace.Populace
import status.Dead


object Main extends App {
  override def main(args: Array[String]): Unit = {
    println("Hello, world!")

    var livingPopulace = Populace.examplePop

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