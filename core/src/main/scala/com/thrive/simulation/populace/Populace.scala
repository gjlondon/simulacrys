package com.thrive.simulation.populace

import com.thrive.simulation.message.Mailbox
import com.thrive.simulation.message.MailboxTypes.Mailbox
import org.joda.time.DateTime
import com.thrive.simulation.person.{Commoner, Person}
import com.thrive.simulation.status.Dead

import scala.collection.generic.CanBuildFrom
import scala.collection.{SetLike, mutable}
import scala.language.postfixOps
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
  def living: Populace = this.filterNot( _.health == Dead )

  def iterator : Iterator[Person] = seq.iterator

  val outgoingMessages: Mailbox = {
    this.foldLeft(Mailbox.empty)((soFar, person) => soFar ++ person.outbox)
  }
}

object Populace {
  def empty: Populace = new Populace()
  def apply(elems: Person*): Populace = (empty /: elems) (_ + _)

  def newBuilder: mutable.Builder[Person, Populace] = new mutable.SetBuilder[Person, Populace](empty)

  implicit def canBuildFrom[A <: Person]: CanBuildFrom[Populace, A, Populace] = new CanBuildFrom[Populace, A, Populace] {
    def apply(from: Populace): mutable.Builder[A, Populace] = newBuilder
    def apply(): mutable.Builder[A, Populace] = newBuilder
  }

  def randomPop(ofSize: Int, startingTime: DateTime): Populace = {
    val popSize = Random.nextInt(10)
    val randomPeople = (0 to popSize) map { idx =>
      Commoner.randomCommoner(asOf = startingTime)
    }

    Populace(randomPeople: _*)
  }

//  def examplePop: Populace = {
//    val bob = Commoner("Bob", FoodInventory(contents = Map[SimpleFood, FoodItemGroup](
//      Beans -> FoodItemGroup.randomAmountOf(Beans),
//      Meat -> FoodItemGroup.randomAmountOf(Meat, max=30))),
//      age = Adult, gender = Male, availableBodyFat = Kilograms(30),
//      leanBodyMass = Kilograms(10),
//      height = Meters(1))
//    val carl = Commoner("Carl", FoodInventory(contents = Map[SimpleFood, FoodItemGroup](
//      Beans -> FoodItemGroup.randomAmountOf(Beans, max=30),
//      Meat -> FoodItemGroup.randomAmountOf(Meat))),
//      age = Adult, gender = Male, availableBodyFat = Kilograms(30),
//      leanBodyMass = Kilograms(10),
//      height = Meters(1))
//    val alice = Commoner("Alice", FoodInventory(contents = Map[SimpleFood, FoodItemGroup](
//      Beans -> FoodItemGroup.randomAmountOf(Beans),
//      Meat -> FoodItemGroup.randomAmountOf(Meat))),
//      age = Adult, gender = Female, availableBodyFat = Kilograms(30),
//      leanBodyMass = Kilograms(10),
//      height = Meters(1))
//
//    Populace(alice, bob, carl)
//  }
}