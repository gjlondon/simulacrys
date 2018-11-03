package populace

import demographic.{Adult, Female, Male}
import inventory.Inventory
import person.{Commoner, Person, PersonNames}
import resource._
import squants.mass.Kilograms

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
      val startingInventory = Inventory(List(Beans(Kilograms(5)), Meat(Kilograms(1))))
      Commoner(PersonNames.nextName, startingInventory, age = Adult, gender = Male)
    }

    Populace(randomPeople: _*)
  }

  def examplePop: Populace = {
    val bob = Commoner("Bob", Inventory(List(Beans(Kilograms(5)), Meat(Kilograms(1)))),
      age = Adult, gender = Male)
    val carl = Commoner("Carl", Inventory(List(Beans(Kilograms(5)), Meat(Kilograms(1)))),
      age = Adult, gender = Male)
    val alice = Commoner("Alice", Inventory(List(Beans(Kilograms(5)), Meat(Kilograms(1)))),
      age = Adult, gender = Female)

    Populace(alice, bob, carl)
  }
}