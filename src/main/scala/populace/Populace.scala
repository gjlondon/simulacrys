package populace

import demographic.{Adult, Female, Male}
import inventory.Inventory
import location.City
import person.{Commoner, Person}
import resource.{Fat, Protein}

import scala.collection.generic.CanBuildFrom
import scala.collection.{SetLike, mutable}


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

  def examplePop: Populace = {
    val waterdeep = City(name = "Waterdeep", latitude = 5, longitude = 5)
    val bob = Commoner("Bob", Inventory.fromManifest(Map(Fat -> 30, Protein -> 20)),
      location = waterdeep, age = Adult, gender = Male)
    val carl = Commoner("Carl", Inventory.fromManifest(Map(Fat -> 20, Protein -> 10)),
      location = waterdeep, age = Adult, gender = Male)
    val alice = Commoner("Alice", Inventory.fromManifest(Map(Fat -> 10, Protein -> 5)),
      location = waterdeep, age = Adult, gender = Female)

    Populace(alice, bob, carl)
  }
}