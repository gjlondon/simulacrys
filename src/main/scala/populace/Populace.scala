package populace

import demographic.{Adult, Female, Male}
import inventory.Inventory
import person.{Commoner, Person, PersonNames}
import resource._
import squants.mass.{Kilograms, Mass}
import squants.motion.Distance
import squants.space.{Centimeters, Meters}

import scala.collection.generic.CanBuildFrom
import scala.collection.{SetLike, mutable}
import scala.util.Random


class Populace(seq : Person*) extends Set[Person]
  with SetLike[Person, Populace]
  with Serializable
 {

  override def empty: Populace = new Populace()

  def + (elem: Person) : Populace = {
    if (seq contains elem) this
    else new Populace(elem +: seq: _*)
  }

  def - (elem: Person) : Populace = {
    if (!(seq contains elem)) this
    else new Populace(seq filterNot (elem ==): _*)
  }

  def contains (elem: Person) : Boolean = seq exists (elem ==)

  def iterator : Iterator[Person] = seq.iterator
}

object Populace {
  def empty: Populace = new Populace()
  def apply(elems: Person*): Populace = (empty /: elems) (_ + _)

  def newBuilder: mutable.Builder[Person, Populace] = new mutable.SetBuilder[Person, Populace](empty)

  implicit def canBuildFrom[A <: Person]: CanBuildFrom[Populace, A, Populace] = new CanBuildFrom[Populace, A, Populace] {
    def apply(from: Populace): mutable.Builder[A, Populace] = newBuilder
    def apply(): mutable.Builder[A, Populace] = newBuilder
  }

  def randomPop(ofSize: Int): Populace = {
    val popSize = Random.nextInt(10)
    val randomPeople = (0 to popSize) map { idx =>
      val startingBeans = FoodItem(sku=Beans, units=Random.nextInt(20))
      val startingMeat = FoodItem(sku=Meat, units=Random.nextInt(20))
      val startingInventory = Inventory(List(startingBeans, startingMeat))
      val startingHeight: Distance = Centimeters(Random.nextGaussian() * 75) + Centimeters(165) // guessing average heights
    val startingLeanMass: Mass = Kilograms(Random.nextGaussian() * 15) + Kilograms(50) // guessing average weights
    val startingFat: Mass = Kilograms(Random.nextGaussian() * 3) + Kilograms(10) // guessing average weights

      Commoner(
        name = PersonNames.nextName,
        inventory = startingInventory,
        age = Adult,
        gender = Male,
        availableBodyFat = startingFat,
        leanBodyMass = startingLeanMass,
        height = startingHeight
      )
    }

    Populace(randomPeople: _*)
  }

  def examplePop: Populace = {
    val bob = Commoner("Bob", Inventory(List(FoodItem(sku=Beans), FoodItem(sku=Meat))),
      age = Adult, gender = Male, availableBodyFat = Kilograms(30),
      leanBodyMass = Kilograms(10),
      height = Meters(1))
    val carl = Commoner("Carl", Inventory(List(FoodItem(sku=Beans), FoodItem(sku=Meat, units=2))),
      age = Adult, gender = Male, availableBodyFat = Kilograms(30),
      leanBodyMass = Kilograms(10),
      height = Meters(1))
    val alice = Commoner("Alice", Inventory(List(FoodItem(sku=Beans, units=2), FoodItem(sku=Meat))),
      age = Adult, gender = Female, availableBodyFat = Kilograms(30),
      leanBodyMass = Kilograms(10),
      height = Meters(1))

    Populace(alice, bob, carl)
  }
}