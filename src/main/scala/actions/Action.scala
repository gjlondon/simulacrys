package actions

import constants.Constants.TICK_DURATION
import facility.Farms
import inventory.FoodInventory
import location.Location
import org.joda.time.DateTime
import person.{Commoner, TypicalTimes}
import resource.{FoodItemGroup, SimpleFood}
import squants.Time
import squants.time.{Hours, Minutes, Seconds}
import status._

import scala.util.Random

object LocalConfig {
  val DEBUG = false
}

trait Volition

case object Voluntary extends Volition
case object Involuntary extends Volition

sealed trait Action {
  def ticksRequired: Int = {
    val secondsInTick: Time = Seconds(TICK_DURATION.seconds)
    Math.ceil(durationToComplete / secondsInTick).toInt
  }

  val durationToComplete: Time
  val name: String
  val exclusive: Boolean
  val interruptable: Boolean
  val volition: Volition
  val instant: Boolean = durationToComplete == Minutes(0)
}

sealed trait PersonAction extends Action
sealed trait PersonReaction extends PersonAction with Reaction

sealed trait PastureAction extends Action
sealed trait PastureReaction extends PastureAction with Reaction

sealed trait FarmAction extends Action
sealed trait FarmReaction extends FarmAction with Reaction

sealed trait ForestAction extends Action
sealed trait ForestReaction extends ForestAction with Reaction

sealed trait Interaction[+T, -U, +V] {
  val preconditionMet: U => Boolean
  val effect: U => V
  val onSuccess:T
  val onFailure: T
  val name: String
}

case class NoOpInteraction(onSuccess: Commoner, onFailure: Commoner) extends Interaction[Commoner, Commoner, Commoner] {
  override val preconditionMet: Commoner => Boolean = _ => true
  override val effect: Commoner => Commoner = { c: Commoner => c }
  override val name: String = "No Op on Person"
}

//case class Till(person: Commoner) extends Interaction[Commoner, facility.Farm, facility.Farm] {
//  override val preconditionMet: facility.Farm => Boolean = {
//    f => f.isAvailable }
//  override val effect: facility.Farm => facility.Farm = {
//    f => f.reserve }
//  override val onSuccess: Commoner = { actions.Farm(person) }
//  override val onFailure: Commoner = { person }
//  override val name: String = "Till the land"
//}

sealed trait Reaction extends Action{
  override val instant: Boolean = true
  override val durationToComplete: Time = Minutes(0)
  override val exclusive: Boolean = true
  override val interruptable: Boolean = false
  override val volition: Volition = Involuntary

}

case object Metabolize extends PersonReaction {
  override val name: String = "Metabolize"
}

object TransitionHealth extends PersonReaction {

  override val name: String = "Update Health"

  def transitionHealth(worseChance: Double, betterChance: Double,
                       starting: HealthStatus): HealthStatus = {
    val roll = Random.nextDouble()
    if (roll <= worseChance) starting.nextWorst
    else if (roll >= betterChance) starting.nextBest
    else starting
  }
}


object CommonerNoAction extends PersonAction {

  override val durationToComplete: Time = Minutes(0)
  override val name: String = "No Action"
  override val exclusive: Boolean = false
  override val interruptable: Boolean = false

  override val volition: Volition = Voluntary
}

case object FarmNoAction extends Action {

  override val durationToComplete: Time = Minutes(0)
  override val name: String = "No Action"
  override val exclusive: Boolean = false
  override val interruptable: Boolean = false

  override val volition: Volition = Voluntary
}


object Farm extends PersonAction {
  override val volition: Volition = Voluntary
  override val durationToComplete: Time = Hours(2)

  def addProduceToInventory(produce: FoodItemGroup,
                            inventory: FoodInventory,
                            cropToFarm: SimpleFood): FoodInventory = {
    val newInventoryContents = inventory.contents.get(cropToFarm) match {
      case None =>
        inventory.contents + (cropToFarm -> produce)
      case Some(existingGroup) =>
        val updatedGroup = existingGroup + produce
        inventory.contents.updated(cropToFarm, updatedGroup)
    }
    inventory.copy(contents = newInventoryContents)
  }

  override val name: String = "Farm"
  override val exclusive: Boolean = true
  override val interruptable: Boolean = true
}

object Eat extends PersonAction {
  override val volition: Volition = Voluntary
  override val durationToComplete: Time = Minutes(30)



  override val name: String = "Eat"
  override val exclusive: Boolean = true
  override val interruptable: Boolean = false
}

object Sleep extends PersonAction {
  override val volition: Volition = Voluntary
  override val durationToComplete: Time = Hours(8)

  override val name: String = "Sleep"
  override val exclusive: Boolean = true
  override val interruptable: Boolean = true
}

object CommonerActions {

  def shouldMetabolize(time: DateTime, location: Location, person: Commoner): Boolean = {
    (TypicalTimes.metabolismHour == time.getHourOfDay) && (TypicalTimes.metabolismMinute == time.getMinuteOfHour)
  }

  def shouldEat(time: DateTime, location: Location, person: Commoner): Boolean = {
    TypicalTimes.mealHours.contains(time.getHourOfDay)
  }

  def shouldFarm(time: DateTime, location: Location, person: Commoner): Boolean = {
    val isLightOut = time.getHourOfDay >= 7 && time.getHourOfDay < 18
    val farmAvailable = location.hasAvailableFacility(Farms)
    isLightOut && person.needsFood && farmAvailable
  }

  def shouldRelax(time: DateTime, location: Location, person: Commoner): Boolean = {
    val isEvening = time.getHourOfDay >= 18 && time.getHourOfDay < 22
    isEvening
  }

  def shouldSleep(time: DateTime, location: Location, person: Commoner): Boolean = {
    val isNight = time.getHourOfDay >= 22 || time.getHourOfDay < 6
    isNight
  }

  def shouldProcreate(time: DateTime, location: Location, person: Commoner): Boolean = {
    val isFunkyTime = time.getHourOfDay == 21
    isFunkyTime && !person.needsFood
  }

  def shouldTransitionHealth(time: DateTime, location: Location, person: Commoner): Boolean = {
    time.getHourOfDay == 3 && time.getMinuteOfHour == 0
  }

  val relax: Commoner => Commoner = { c: Commoner => c }
  // TODO handle procreation
  val procreate: Commoner => Commoner = { c: Commoner => c }
  val sleep: Commoner => Commoner = { c: Commoner => c }
  val party: Commoner => Commoner = { c: Commoner =>
    relax(c)
  }
  val unit: Commoner => Commoner = { c: Commoner => c }

//  def farmInteraction(person: Commoner,
//                      location: Location,
//                      ): Option[PersonToFacilityMessage] = {
//    val interaction = Till(person)
//    val targetFarm = location.findAvailableFacility(Farms)
//    targetFarm match {
//      case None => None
//      case Some(farm) => Some(PersonToFacilityMessage(from = person, to = farm,
//        payload = interaction))
//    }
//
//  }
}
