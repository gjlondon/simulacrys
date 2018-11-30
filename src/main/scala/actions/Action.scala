package actions

import meal.Meal
import org.joda.time.DateTime
import person.{ActionCandidates, Commoner, Person, TypicalTimes}
import resource.{FoodItemGroup, Fresh, Freshness, SimpleFood}
import squants.Time
import squants.mass.Kilograms
import status.Dead
import world.World
import constants.Constants.CALORIES_PER_KILO_OF_FAT
import squants.time.{Hours, Minutes}

trait Action[T <: Commoner] {
  val durationToComplete: Time
  def apply(person: T): T
  val name: String
  val exclusive: Boolean
  val interruptable: Boolean
}

trait Performance[T <: Commoner] {
  val of: Action[T]
  val timeRemaining: Time
}

object Metabolize extends Action[Commoner] {

  override val durationToComplete: Time = Minutes(1)
  override val name: String = "Metabolize"
  override val exclusive: Boolean = false
  override val interruptable: Boolean = false

  override def apply(person: Commoner): Commoner = {
    val fatBurned = person.caloriesRequired / CALORIES_PER_KILO_OF_FAT

    person.copy (
      availableBodyFat = person.availableBodyFat - fatBurned,
      health = if (person.availableBodyFat <= Kilograms(0)) Dead else person.health
    )
  }
}

object Farm extends Action[Commoner] {
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
    val newGroup = FoodItemGroup(Map[Freshness, Int](Fresh -> cropYield), sku=cropToFarm)

    val newInventoryContents = person.inventory.contents.get(cropToFarm) match {
      case None =>
        person.inventory.contents + (cropToFarm -> newGroup)
      case Some(existingGroup) =>
        val updatedGroup = existingGroup + newGroup
        person.inventory.contents.updated(cropToFarm, updatedGroup)
    }
    val newInventory = person.inventory.copy(contents = newInventoryContents)
    person.copy(inventory = newInventory)
  }

  override val name: String = "Farm"
  override val exclusive: Boolean = true
  override val interruptable: Boolean = true
}

object Eat extends Action[Commoner] {
  override val durationToComplete: Time = Minutes(30)

  override def apply(person: Commoner): Commoner = {
    val meal = Meal.cheapestMeal(
      candidateComponents = person.inventory,
      requiredCalories = person.caloriesRequired
    )

    meal match {
      case None => person
      case Some(eatenMeal) =>
        val fatGained = eatenMeal.calories / CALORIES_PER_KILO_OF_FAT
        person.copy(
          inventory = person.inventory.deductMeal(eatenMeal),
          availableBodyFat = person.availableBodyFat + fatGained
        )
    }
  }

  override val name: String = "Eat"
  override val exclusive: Boolean = true
  override val interruptable: Boolean = false
}

object CommonerActions {

  def shouldMetabolize(time: DateTime, world: World, person: Commoner): Boolean = {
    TypicalTimes.metabolismHour == time.getHourOfDay
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

  val candidateActions: ActionCandidates = List(
    (Metabolize, shouldMetabolize),
    (Eat, shouldEat),
    (Farm, shouldFarm),
//    ("relax", relax, shouldRelax),
//    ("procreate", procreate, shouldProcreate),
//    ("sleep", sleep, shouldSleep)
  )
}
