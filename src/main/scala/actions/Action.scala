package actions

import constants.Constants.{ENERGY_PER_KILO_OF_FAT, TICK_DURATION}
import entity.Entity
import facility.{Facility, Farms}
import inventory.FoodInventory
import location.Location
import meal.Meal
import org.joda.time.DateTime
import person.{ActionCandidates, Commoner, ReactionCandidates, TypicalTimes}
import resource.{FoodItemGroup, Fresh, Freshness, SimpleFood}
import squants.Time
import squants.mass.Kilograms
import squants.time.{Hours, Minutes, Seconds}
import status._

import scala.util.Random

object LocalConfig {
  val DEBUG = false
}

import actions.LocalConfig.DEBUG

trait Volition

case object Voluntary extends Volition
case object Involuntary extends Volition

sealed trait Action[T <: Entity] {
  def ticksRequired: Int = {
    val secondsInTick: Time = Seconds(TICK_DURATION.seconds)
    Math.ceil(durationToComplete / secondsInTick).toInt
  }

  val durationToComplete: Time
  def apply(person: T): T
  val name: String
  val exclusive: Boolean
  val interruptable: Boolean
  val volition: Volition
  val instant: Boolean = durationToComplete == Minutes(0)
}

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

case class Till(person: Commoner) extends Interaction[Commoner, facility.Farm, facility.Farm] {
  override val preconditionMet: facility.Farm => Boolean = {
    f => f.isAvailable }
  override val effect: facility.Farm => facility.Farm = {
    f => f.reserve }
  override val onSuccess: Commoner = { actions.Farm(person) }
  override val onFailure: Commoner = { person }
  override val name: String = "Till the land"
}

sealed trait Reaction[T <: Entity] extends Action[T] {
  override val instant: Boolean = true
  override val durationToComplete: Time = Minutes(0)
  override val exclusive: Boolean = true
  override val interruptable: Boolean = false
  override val volition: Volition = Involuntary

}

object Metabolize extends Reaction[Commoner] {

  override val name: String = "Metabolize"

  override def apply(person: Commoner): Commoner = {
    val fatBurned = person.foodEnergyRequired / ENERGY_PER_KILO_OF_FAT

    val updatedBodyFat = person.availableBodyFat - fatBurned
    val updatedHealth = if (updatedBodyFat <= Kilograms(0)) {
      if (DEBUG) {
        println(s"${person.name} died because body fat fell to $updatedBodyFat")
      }
      Dead
    } else person.health

    if (updatedHealth == Dead) println(s"${person.name} died from starvation")

    person.copy (
      availableBodyFat = updatedBodyFat,
      health = updatedHealth
    )
  }
}

object TransitionHealth extends Reaction[Commoner] {

  override val name: String = "Update Health"

  def transitionHealth(worseChance: Double, betterChance: Double,
                       starting: HealthStatus): HealthStatus = {
    val roll = Random.nextDouble()
    if (roll <= worseChance) starting.nextWorst
    else if (roll >= betterChance) starting.nextBest
    else starting
  }

  override def apply(person: Commoner): Commoner = {

    val (worseChance, betterChance) = HealthStatus.transitionProbabilities(person.ageBracket)

    val nextHealth = transitionHealth(worseChance = worseChance,
      betterChance = betterChance,
      starting = person.health)

    if (nextHealth == Dead) println(s"${person.name} died in a health event")
    person.copy (
      health = nextHealth
    )
  }
}


object CommonerNoAction extends Action[Commoner] {

  override val durationToComplete: Time = Minutes(0)
  override val name: String = "No Action"
  override val exclusive: Boolean = false
  override val interruptable: Boolean = false

  override def apply(person: Commoner): Commoner = {
    person
  }

  override val volition: Volition = Voluntary
}

case object FarmNoAction extends Action[facility.Farm] {

  override val durationToComplete: Time = Minutes(0)
  override val name: String = "No Action"
  override val exclusive: Boolean = false
  override val interruptable: Boolean = false

  override def apply(entity: facility.Farm): facility.Farm = {
    entity
  }

  override val volition: Volition = Voluntary
}


object Farm extends Action[Commoner] {
  override val volition: Volition = Voluntary
  override val durationToComplete: Time = Hours(2)

  override def apply(person: Commoner): Commoner = {
    /**
    People can take advantage of local available arable land to produce a certain quantity of
    crops and/or live stock.

      We'd like to be able to select a subset of possible crops ot farm, and then randomly generate a crop yield
      amount based on a distribution that is specific to each type of crop. Ideally we could create a "yield map"
      which would map from Type to Yield amount.

      Ideally farms would have some degree of persistence so that we could model the fact that growing crops inevitably takes
      time, but maybe that's a future feature.

      */

    val cropToFarm: SimpleFood = SimpleFood.randomCrop()

    val cropYield: Int = cropToFarm.randomYield
    val produce = FoodItemGroup(Map[Freshness, Int](Fresh -> cropYield), sku=cropToFarm)

    val newInventory = addProduceToInventory(produce = produce, inventory = person.inventory,
      cropToFarm = cropToFarm)
    if (DEBUG) {
      println(s"Yield $produce")
      println(s"old inventory ${person.name} ${person.inventory.contents.get(cropToFarm)}; new inventory ${newInventory.contents.get(cropToFarm)}")
    }
    person.copy(inventory = newInventory)
  }

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

object Eat extends Action[Commoner] {
  override val volition: Volition = Voluntary
  override val durationToComplete: Time = Minutes(30)

  override def apply(person: Commoner): Commoner = {
    val meal = Meal.cheapestMeal(
      candidateComponents = person.inventory,
      requiredEnergy = person.foodEnergyRequired
    )

    meal match {
      case None =>
        if (DEBUG) {
          println(s"No meal could be constructed from ${person.inventory}")
        }
        person
      case Some(eatenMeal) =>
        val fatGained = eatenMeal.energy / ENERGY_PER_KILO_OF_FAT
        val newBodyFat = person.availableBodyFat + fatGained
        val newInventory = person.inventory.deductMeal(eatenMeal)

        if (DEBUG) println(f"Yum meal for ${person.name} ${eatenMeal.kilocalories}%1.3f new fat $fatGained to reach $newBodyFat." +
          f"Remaining inventory has ${newInventory.totalAvailableCalories}%1.3f")

        person.copy(
          inventory = newInventory,
          availableBodyFat = newBodyFat
        )
    }
  }

  override val name: String = "Eat"
  override val exclusive: Boolean = true
  override val interruptable: Boolean = false
}

object Sleep extends Action[Commoner] {
  override val volition: Volition = Voluntary
  override val durationToComplete: Time = Hours(8)

  override def apply(person: Commoner): Commoner = {
    person
  }

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
