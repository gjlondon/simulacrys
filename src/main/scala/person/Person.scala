package person

import java.util.UUID

import actions._
import com.github.nscala_time.time.Imports._
import configuration.Configuration.DEBUG
import demographic._
import entity.Entity
import inventory.FoodInventory
import location.Location
import message.MailboxTypes.{Inbox, Mailbox, Outbox}
import message._
import org.joda.time.DateTime
import person.Handlers.{CommonerReplyHandlers, emptyReplyHandler}
import resource.Calorie.calorie
import resource.{Beans, FoodItemGroup, Meat, SimpleFood}
import squants.energy.Energy
import squants.mass._
import squants.motion.Distance
import squants.space.Centimeters
import status._

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
  override type Specific = Commoner

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

object Handlers {
  type CommonerReplyHandlers = Map[UUID, (Commoner => Commoner, Commoner => Commoner)]

  val emptyReplyHandler: CommonerReplyHandlers = {
    Map[UUID, (Commoner => Commoner, Commoner => Commoner)]()
  }
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
                    inbox: Inbox = Mailbox.empty,
                    outbox: Outbox = Mailbox.empty,
                    replyHandlers: CommonerReplyHandlers = emptyReplyHandler
                   )
  extends Person {
  override type Specific = Commoner

  import actions.CommonerActions.{candidateActions, involuntaryActions}
  import person.Handlers.CommonerReplyHandlers

  override def receiveMessages(messages: Queue[Message]): Commoner = {
    this.copy(inbox = inbox ++ messages)
  }

  override def handleInbox(entity: Commoner): (Commoner, Outbox) = {
    val (updatedEntity, outbox) = consumeInbox(inbox = entity.inbox, entity = entity)
    (updatedEntity.copy(inbox = Mailbox.empty), outbox)
  }

  def requestSucceeds(payload: MessagePayload, entity: Commoner): Boolean = {
    payload match {
      case NoOp => true
    }
  }

  def onRequestSuccess(payload: MessagePayload, entity: Commoner): Commoner = {
    payload match {
      case NoOp => entity
    }
  }

  def onRequestFailure(payload: MessagePayload, entity: Commoner): Commoner = {
    payload match {
      case NoOp => entity
    }
  }

  def update(time: DateTime, location: Location): Commoner =  {
    val noOpNotReq = Request(
      from = this.address, to = this.address,
      payload = NoOp,
      //      condition = {
      //        case _: Farm => true
      //        case _ => false
      //      },
      //      onSuccess = (c: Farm) => c,
      //      onFailure = (c: Farm) => c,
    )

    val testInbox = inbox.enqueue(noOpNotReq).enqueue(noOpNotReq).enqueue(noOpNotReq)

    // 1. process all messages in inbox, updating state as necessary

    // 2. if appropriate, create some messages to send to entities with whom
    // you want to interact. These entities may include yourself, who e.g might
    // complete some action on the next tick that has been started this tick

    // 3. create a set of outgoing replies that respond to any income messages
    // (to allow senders to react to success or failure), or that initiate an
    // interaction with another entity

    val (inboxIncorporated, outbox) = handleInbox(
      entity = this.copy(inbox = testInbox),
    )


    val afterReactions: Commoner = react(time, location, inboxIncorporated)

    // if person is not incapacitated, allow a voluntary action
    // by assumption, no action is allowed to take less than the length of a single tick
    // so it's safe to assume that in a given tick, a person will take at most one action
    val afterActions = act(time, location, afterReactions)

    afterActions.copy(asOf = time, outbox = outbox)
  }

  def act(time: DateTime, location: Location, person: Commoner): Commoner =  {
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
        val (action, maybeMessages) = selectAction(time, location, person = person, candidates = candidateActions)
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
  private def selectAction(datetime: DateTime, location: Location,
                           person: Commoner,
                           candidates: ActionCandidates): (
    Action[Commoner], Option[Mailbox]
    ) = {
    candidates match {
      case Nil => (CommonerNoAction, None)
      case (candidateAction, condition, interactionGenerator) :: remainingCandidates =>
        val shouldAct = condition(datetime, location, person)
        if (DEBUG && shouldAct) {
          val msg = s"Person ${person.name} should perform ${candidateAction.name} at time $datetime"
          println(msg)
        }
        if (shouldAct) {
          (candidateAction, None)
//          interactionGenerator match {
//            case None => (candidateAction, None)
//            case Some(interactions) =>
//              val maybeMessages = interactions(person, location)
//              maybeMessages match {
//                case None => (candidateAction, None)
//                case Some(message) => (candidateAction, Some(message))
//              }
//          }
        }
        else selectAction(
          datetime, location,
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

