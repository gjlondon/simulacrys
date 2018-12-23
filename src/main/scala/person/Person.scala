package person

import actions._
import configuration.Configuration.DEBUG
import demographic._
import entity.Entity
import inventory.FoodInventory
import message.Message
import org.joda.time.DateTime
import resource.Calorie.calorie
import squants.Time
import squants.energy.Energy
import squants.mass._
import squants.motion.Distance
import squants.time.Hours
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
  val age: AgeBracket
  val gender: Gender
  val height: Distance
  val leanBodyMass: Mass
  val availableBodyFat: Mass

  def weight: Mass = leanBodyMass + availableBodyFat
  def bodyMassIndex: AreaDensity = calcBodyMassIndex(height, weight)
  def weightStatus: WeightStatus = calcWeightStatus(bodyMassIndex)
  def act(time: DateTime, world: World): Person

  val foodEnergyRequired: Energy = {
    val required: Int = (age: AgeBracket, gender) match {
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
                    age: AgeBracket,
                    gender: Gender,
                    height: Distance,
                    availableBodyFat: Mass,
                    leanBodyMass: Mass,
                    health: HealthStatus = Fine,
                    currentActivity: CurrentActivity = Idle,
                    actionQueue: Queue[Action[Commoner]] = Queue.empty[Action[Commoner]],
                    inbox: Queue[Message] = Queue.empty[Message],
                    outbox: Queue[Message] = Queue.empty[Message]
                   )
  extends Person {

  import actions.CommonerActions.candidateActions

  def act(time: DateTime, world: World): Commoner =  {
    // by assumption, no action is allowed to take less than the length of a single tick
    // so it's safe to assume that in a given tick, a person will take at most one action

    currentActivity match {
      case Incapacitated =>
        if (DEBUG) println(s"${this.name} incapacit")
        this
      case performance: CommonerPerformance =>
        if (DEBUG) println(s"${this.name} still working on $performance, ticks remaining ${performance.ticksRemaining}")
        perform(performance, person = this)
      case Idle =>
        val action = selectAction(time, world, person = this, candidates = candidateActions)
        val performance = CommonerPerformance(perform = action)
        if (DEBUG) println(s"${this.name} idle,  starting $action will take ${performance.ticksRemaining}")

        perform(performance, person = this)
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

