package actions

import constants.Constants.{ENERGY_PER_KILO_OF_FAT, TICK_DURATION}
import inventory.FoodInventory
import meal.Meal
import org.joda.time.DateTime
import person.{ActionCandidates, Commoner, TypicalTimes}
import resource.{FoodItemGroup, Fresh, Freshness, SimpleFood}
import squants.Time
import squants.mass.Kilograms
import squants.time.{Hours, Minutes, Seconds}
import status.Dead
import world.World

object LocalConfig {
  val DEBUG = false
}

import actions.LocalConfig.DEBUG

trait Volition

case object Voluntary extends Volition
case object Involuntary extends Volition

trait Action[T <: Commoner] {
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
}



object NoAction extends Action[Commoner] {

  override val durationToComplete: Time = Minutes(0)
  override val name: String = "No Action"
  override val exclusive: Boolean = false
  override val interruptable: Boolean = false

  override def apply(person: Commoner): Commoner = {
    person
  }

  override val volition: Volition = Voluntary
}

object Metabolize extends Action[Commoner] {

  override val durationToComplete: Time = Minutes(1)
  override val name: String = "Metabolize"
  override val exclusive: Boolean = false
  override val interruptable: Boolean = false
  override val volition: Volition = Involuntary


  override def apply(person: Commoner): Commoner = {
    val fatBurned = person.foodEnergyRequired / ENERGY_PER_KILO_OF_FAT

    val updatedBodyFat = person.availableBodyFat - fatBurned
    val updatedHealth = if (updatedBodyFat <= Kilograms(0)) {
      if (DEBUG) {
        println(s"${person.name} died because body fat fell to $updatedBodyFat")
      }
      Dead
    } else person.health
    person.copy (
      availableBodyFat = updatedBodyFat,
      health = updatedHealth
    )
  }
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

object CommonerActions {

  def shouldMetabolize(time: DateTime, world: World, person: Commoner): Boolean = {
    (TypicalTimes.metabolismHour == time.getHourOfDay) && (TypicalTimes.metabolismMinute == time.getMinuteOfHour)
  }

  def shouldEat(time: DateTime, world: World, person: Commoner): Boolean = {
    TypicalTimes.mealHours.contains(time.getHourOfDay)
  }

  def shouldFarm(time: DateTime, world: World, person: Commoner): Boolean = {
    val isLightOut = time.getHourOfDay >= 7 && time.getHourOfDay < 18
    isLightOut && person.needsFood
  }

  def shouldRelax(time: DateTime, world: World, person: Commoner): Boolean = {
    val isEvening = time.getHourOfDay >= 18 && time.getHourOfDay < 22
    isEvening
  }

  def shouldSleep(time: DateTime, world: World, person: Commoner): Boolean = {
    val isNight = time.getHourOfDay >= 22 || time.getHourOfDay < 7
    isNight
  }

  def shouldProcreate(time: DateTime, world: World, person: Commoner): Boolean = {
    val isFunkyTime = time.getHourOfDay == 21
    isFunkyTime && !person.needsFood
  }

  val relax: Commoner => Commoner = { c: Commoner => c }
  // TODO handle procreation
  val procreate: Commoner => Commoner = { c: Commoner => c }
  val sleep: Commoner => Commoner = { c: Commoner => c }
  val party: Commoner => Commoner = { c: Commoner =>
    relax(c)
  }
  val unit: Commoner => Commoner = { c: Commoner => c }

  val candidateActions: ActionCandidates = List(
    (Metabolize, shouldMetabolize),
    (Eat, shouldEat),
    (Farm, shouldFarm),
//    ("relax", relax, shouldRelax),
//    ("procreate", procreate, shouldProcreate),
//    ("sleep", sleep, shouldSleep)
  )
}
