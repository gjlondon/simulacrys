package simulation.actions

import simulation.constants.Constants.TICK_DURATION
import simulation.facility.Farms
import simulation.inventory.FoodInventory
import simulation.location.Location
import org.joda.time.DateTime
import simulation.person.{Commoner, TypicalTimes}
import simulation.resource.{FoodItemGroup, SimpleFood}
import squants.Time
import squants.time.{Hours, Minutes, Seconds}
import simulation.status._

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

sealed trait NoAction extends Action {
  override val durationToComplete: Time = Minutes(0)
  override val name: String = "No Action"
  override val exclusive: Boolean = false
  override val interruptable: Boolean = false
  override val volition: Volition = Voluntary

}

sealed trait PersonAction extends Action
case object PersonNoAction extends PersonAction with NoAction
sealed trait PersonReaction extends PersonAction with Reaction

sealed trait PastureAction extends Action
case object PastureNoAction extends PastureAction with NoAction
sealed trait PastureReaction extends PastureAction with Reaction

sealed trait FarmAction extends Action
case object FarmNoAction extends FarmAction with NoAction
sealed trait FarmReaction extends FarmAction with Reaction

sealed trait ForestAction extends Action
case object ForestNoAction extends ForestAction with NoAction
sealed trait ForestReaction extends ForestAction with Reaction

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
}
