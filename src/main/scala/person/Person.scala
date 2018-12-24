package person

import actions._
import com.github.nscala_time.time.Imports._
import configuration.Configuration.DEBUG
import demographic._
import entity.Entity
import inventory.FoodInventory
import message.Message
import org.joda.time.DateTime
import resource.Calorie.calorie
import resource.{Beans, FoodItemGroup, Meat, SimpleFood}
import squants.energy.Energy
import squants.mass._
import squants.motion.Distance
import squants.space.Centimeters
import status._
import world.World

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

sealed trait WeightStatus

case object Underweight extends WeightStatus
case object Normal extends WeightStatus
case object Overweight extends WeightStatus
case object Obese extends WeightStatus
case object DangerouslyLow extends WeightStatus

package object healthCalculations {
    def calcWeightStatus (fromBodyMassIndex: AreaDensity): WeightStatus = {
      fromBodyMassIndex match {
        case bmi if bmi < KilogramsPerSquareMeter(13) => DangerouslyLow // https://www.livestrong.com/article/434699-what-is-a-dangerously-low-bmi/
        case bmi if bmi < KilogramsPerSquareMeter(18.5) => Underweight
        case bmi if bmi < KilogramsPerSquareMeter(24.9) => Normal
        case bmi if bmi < KilogramsPerSquareMeter(29.9) => Overweight
        case _ => Obese
      }
    }

  def calcBodyMassIndex(height: Distance, weight: Mass): AreaDensity = weight / (height * height)
}

import person.healthCalculations.{calcBodyMassIndex, calcWeightStatus}

sealed trait Person extends Entity {
  val name: String
  val inventory: FoodInventory
  val health: HealthStatus
  val birthDate: DateTime
  val asOf: DateTime
  val gender: Gender
  val height: Distance
  val leanBodyMass: Mass
  val availableBodyFat: Mass

  val age: Int = (birthDate to asOf).toPeriod.getYears

  val ageBracket: AgeBracket = {

    age match {
      case x if x < 13 => Child
      case x if x < 18 => Young
      case x if x < 60 => Adult
      case _ => Old
    }
  }

  def weight: Mass = leanBodyMass + availableBodyFat
  def bodyMassIndex: AreaDensity = calcBodyMassIndex(height, weight)
  def weightStatus: WeightStatus = calcWeightStatus(bodyMassIndex)
  def update(time: DateTime, world: World): Person

  val foodEnergyRequired: Energy = {
    val required: Int = (ageBracket: AgeBracket, gender) match {
      case (Child, Female) => 1500
      case (Child, Male) => 1600
      case (Young, Female) => 1700
      case (Young, Male) => 1800
      case (Adult, Female) => 1900
      case (Adult, Male) => 2000
      case (demographic.Old, Female) => 1800
      case (demographic.Old, Male) => 1900
    }

    required * calorie
  }

  val needsFood: Boolean = inventory.totalAvailableFoodEnergy < foodEnergyRequired * 10
}

object TypicalTimes {
  val mealHours: Set[Int] = Set(8, 12, 18)
  val metabolismHour = 7
  val metabolismMinute = 0
}

case class Commoner(name: String,
                    inventory: FoodInventory,
                    gender: Gender,
                    height: Distance,
                    availableBodyFat: Mass,
                    leanBodyMass: Mass,
                    asOf: DateTime,
                    birthDate: DateTime,
                    health: HealthStatus = Fine,
                    currentActivity: CurrentActivity = Idle,
                    actionQueue: Queue[Action[Commoner]] = Queue.empty[Action[Commoner]],
                    inbox: Queue[Message] = Queue.empty[Message],
                    outbox: Queue[Message] = Queue.empty[Message]
                   )
  extends Person {
  import actions.CommonerActions.{candidateActions, involuntaryActions}

  def update(time: DateTime, world: World): Commoner =  {

    val reacted: Commoner = react(time, world)

    // if person is not incapacitated, allow a voluntary action
    // by assumption, no action is allowed to take less than the length of a single tick
    // so it's safe to assume that in a given tick, a person will take at most one action
    val acted = act(time, world, reacted)

    acted.copy(asOf = time)
  }

  private def react(time: DateTime, world: World): Commoner = {
    // resolve involuntary actions
    // TODO add a concept of thirst

    performNextReaction(datetime = time, world = world,
      person = this, reactions = involuntaryActions)
  }

  @tailrec
  private def performNextReaction(datetime: DateTime, world: World,
                                  person: Commoner,
                                  reactions: ActionCandidates): Commoner = {
    reactions match {
      case Nil => person
      case (possibleReaction, condition) :: remainingCandidates =>
        val shouldReact = condition(datetime, world, person)
        if (DEBUG && shouldReact) {
          val msg = s"Person ${person.name} should perform ${possibleReaction.name} at time $datetime"
          println(msg)
        }
        val action = if (shouldReact) possibleReaction else NoAction
        performNextReaction(
          datetime, world,
          action(person),
          remainingCandidates,
        )
    }
  }

  def act(time: DateTime, world: World, person: Commoner): Commoner =  {
    // by assumption, no action is allowed to take less than the length of a single tick
    // so it's safe to assume that in a given tick, a person will take at most one action

    person.currentActivity match {
      case Incapacitated =>
        if (DEBUG) println(s"${person.name} incapacit")
        person
      case performance: CommonerPerformance =>
        if (DEBUG) println(s"${person.name} still working on $performance, ticks remaining ${performance.ticksRemaining}")
        perform(performance, person = person)
      case Idle =>
        val action = selectAction(time, world, person = person, candidates = candidateActions)
        val performance = CommonerPerformance(perform = action)
        if (DEBUG) println(s"${person.name} idle,  starting $action will take ${performance.ticksRemaining}")

        perform(performance, person = person)
    }
  }

  def perform(performance: CommonerPerformance, person: Commoner): Commoner = {
    val progressed = performance.progress
    val (nextPerson, nextActivity) =
      if (progressed.isComplete) {
        if (DEBUG) println(s"$performance complete")
        (progressed.perform(person = person), Idle)
      }
      else (person, progressed)
    nextPerson.copy(currentActivity = nextActivity)
  }

  @tailrec
  private def selectAction(datetime: DateTime, world: World,
                           person: Commoner,
                           candidates: ActionCandidates): Action[Commoner] = {
    candidates match {
      case Nil => NoAction
      case (candidateAction, condition) :: remainingCandidates =>
        val shouldAct = condition(datetime, world, person)
        if (DEBUG && shouldAct) {
          val msg = s"Person ${person.name} should perform ${candidateAction.name} at time $datetime"
          println(msg)
        }
        if (shouldAct) candidateAction
        else selectAction(
          datetime, world,
          person,
          remainingCandidates,
        )
    }
  }
}

object Commoner {
  def randomCommoner(asOf: DateTime): Commoner = {
    val startingInventory = FoodInventory(
      contents = Map[SimpleFood, FoodItemGroup](
        Beans -> FoodItemGroup.randomAmountOf(Beans),
        Meat -> FoodItemGroup.randomAmountOf(Meat)))
    val startingHeight: Distance = Centimeters(Random.nextGaussian() * 75) + Centimeters(165) // guessing average heights
    val startingLeanMass: Mass = Kilograms(Random.nextGaussian() * 15) + Kilograms(50) // guessing average weights
    val startingFat: Mass = Kilograms(Random.nextGaussian() * 3) + Kilograms(10) // guessing average weights
    val startingAge = Random.nextInt(90)
    val birthDate = asOf - startingAge.years

    Commoner(
      name = PersonNames.nextName,
      inventory = startingInventory,
      asOf = asOf,
      birthDate = birthDate,
      gender = Male,
      availableBodyFat = startingFat,
      leanBodyMass = startingLeanMass,
      height = startingHeight
    )
  }
}

object PersonNames {
  def nextName: String = names(Random.nextInt(names.length))

  val names: Vector[String] = Vector(
    "Abrielle",
    "Adair",
    "Adara",
    "Adriel",
    "Aiyana",
    "Alissa",
    "Alixandra",
    "Altair",
    "Amara",
    "Anatola",
    "Anya",
    "Arcadia",
    "Ariadne",
    "Arianwen",
    "Aurelia",
    "Aurelian",
    "Aurelius",
    "Avalon",
    "Acalia",
    "Alaire",
    "Auristela",
    "Bastian",
    "Breena",
    "Brielle",
    "Briallan",
    "Briseis",
    "Cambria",
    "Cara",
    "Carys",
    "Caspian",
    "Cassia",
    "Cassiel",
    "Cassiopeia",
    "Cassius",
    "Chaniel",
    "Cora",
    "Corbin",
    "Cyprian",
    "Daire",
    "Darius",
    "Destin",
    "Drake",
    "Drystan",
    "Dagen",
    "Devlin",
    "Devlyn",
    "Eira",
    "Eirian",
    "Elysia",
    "Eoin",
    "Evadne",
    "Eliron",
    "Evanth",
    "Fineas",
    "Finian",
    "Fyodor",
    "Gareth",
    "Gavriel",
    "Griffin",
    "Guinevere",
    "Gaerwn",
    "Ginerva",
    "Hadriel",
    "Hannelore",
    "Hermione",
    "Hesperos",
    "Iagan",
    "Ianthe",
    "Ignacia",
    "Ignatius",
    "Iseult",
    "Isolde",
    "Jessalyn",
    "Kara",
    "Kerensa",
    "Korbin",
    "Kyler",
    "Kyra",
    "Katriel",
    "Kyrielle",
    "Leala",
    "Leila",
    "Lilith",
    "Liora",
    "Lucien",
    "Lyra",
    "Leira",
    "Liriene",
    "Liron",
    "Maia",
    "Marius",
    "Mathieu",
    "Mireille",
    "Mireya",
    "Maylea",
    "Meira",
    "Natania",
    "Nerys",
    "Nuriel",
    "Nyssa",
    "Neirin",
    "Nyfain",
    "Oisin",
    "Oralie",
    "Orion",
    "Orpheus",
    "Ozara",
    "Oleisa",
    "Orinthea",
    "Peregrine",
    "Persephone",
    "Perseus",
    "Petronela",
    "Phelan",
    "Pryderi",
    "Pyralia",
    "Pyralis",
    "Qadira",
    "Quintessa",
    "Quinevere",
    "Raisa",
    "Remus",
    "Rhyan",
    "Rhydderch",
    "Riona",
    "Renfrew",
    "Saoirse",
    "Sarai",
    "Sebastian",
    "Seraphim",
    "Seraphina",
    "Sirius",
    "Sorcha",
    "Saira",
    "Sarielle",
    "Serian",
    "Tavish",
    "Tearlach",
    "Terra",
    "Thalia",
    "Thaniel",
    "Theia",
    "Torian",
    "Torin",
    "Tressa",
    "Tristana",
    "Uriela",
    "Urien",
    "Ulyssia",
    "Vanora",
    "Vespera",
    "Vasilis",
    "Xanthus",
    "Xara",
    "Xylia",
    "Yadira",
    "Yseult",
    "Yakira",
    "Yeira",
    "Yeriel",
    "Yestin",
    "Zaira",
    "Zephyr",
    "Zora",
    "Zorion",
    "Zaniel",
    "Zarek")
}

