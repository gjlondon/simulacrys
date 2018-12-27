package person

import java.util.UUID
import java.util.UUID.randomUUID

import actions.CommonerActions._
import actions.Farm.addProduceToInventory
import actions.TransitionHealth.transitionHealth
import actions._
import com.github.nscala_time.time.Imports._
import constants.Constants.ENERGY_PER_KILO_OF_FAT
import demographic._
import entity.Entity
import inventory.FoodInventory
import location.Location
import meal.Meal
import message.MailboxTypes.{Inbox, Mailbox, Outbox}
import message._
import org.joda.time.DateTime
import person.Handlers.{CommonerReplyHandlers, emptyReplyHandler}
import resource.Calorie.calorie
import resource.{Old => _, _}
import squants.energy.Energy
import squants.mass._
import squants.motion.Distance
import squants.space.Centimeters
import status._

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

object LocalConfig {
  val DEBUG = false
}

import actions.LocalConfig.DEBUG

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
                    actionQueue: Queue[PersonAction] = Queue[PersonAction](),
                    inbox: Inbox = Mailbox.empty,
                    outbox: Outbox = Mailbox.empty,
                    replyHandlers: CommonerReplyHandlers = emptyReplyHandler,
                    address: UUID = randomUUID()
                   )
  extends Person {
  override type Specific = Commoner
  override type RelevantAction = PersonAction

  override def receiveMessages(messages: Queue[Message]): Commoner = {
    this.copy(inbox = inbox ++ messages)
  }

  override def handleInbox(entity: Commoner): (Commoner, Outbox) = {
    val (updatedEntity, outbox) = consumeInbox(inbox = entity.inbox, entity = entity)
    (updatedEntity.copy(inbox = Mailbox.empty), outbox)
  }

  override def handleReply(reply: Reply,
                           entity: Commoner): Commoner = {
    val confirmsHandled: Commoner = handleConfirmations(reply, entity)

    replyHandlers.get(reply.uuid) match {
      case None => confirmsHandled
      case Some((onSuccess, onFailure)) =>
        if (reply.succeeded) onSuccess(confirmsHandled) else onFailure(confirmsHandled)
    }
  }

  private def handleConfirmations(reply: Reply, entity: Commoner): Commoner = {
    val confirmsHandled: Commoner = currentActivity match {
      case p: CommonerPerformance if p.pendingConfirms.contains(reply.re) =>
        if (!reply.succeeded) {
          // TODO allow some other consequences of failure?
          entity.copy(currentActivity = Idle)
        }
        else {
          val pendingPerformance: CommonerPerformance = p.copy(
            pendingConfirms = p.pendingConfirms - reply.re
          )
          entity.copy(currentActivity = pendingPerformance)
        }
      case _ => entity
    }
    confirmsHandled
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


    // 1. process all messages in inbox, updating state as necessary

    // 2. if appropriate, create some messages to send to entities with whom
    // you want to interact. These entities may include yourself, who e.g might
    // complete some action on the next tick that has been started this tick

    // 3. create a set of outgoing replies that respond to any income messages
    // (to allow senders to react to success or failure), or that initiate an
    // interaction with another entity

    val (inboxIncorporated, outbox) = handleInbox(
      entity = this,
    )

    val (afterReactions: Commoner, reactedOutbox: Outbox) = react(time, location, inboxIncorporated)

    // if person is not incapacitated, allow a voluntary action
    // by assumption, no action is allowed to take less than the length of a single tick
    // so it's safe to assume that in a given tick, a person will take at most one action
    val (afterActions, actedOutbox) = act(time, location, afterReactions)

    //    if (actedOutbox.nonEmpty) println(actedOutbox)
    afterActions.copy(asOf = time, outbox = outbox ++ reactedOutbox ++ actedOutbox)
  }

  def act(time: DateTime, location: Location, entity: Commoner): (Commoner, Outbox) =  {
    // by assumption, no action is allowed to take less than the length of a single tick
    // so it's safe to assume that in a given tick, a person will take at most one action

    entity.currentActivity match {
      case Incapacitated =>
        if (DEBUG) println(s"${entity.name} incapacit")
        (entity, entity.outbox)
      case performance: CommonerPerformance =>
        if (DEBUG) println(s"${entity.name} still working on $performance, ticks remaining ${performance.ticksRemaining}")
        perform(performance, person = entity)
      case Idle =>
        // an entity can initiate an action, which will consume at least one tick, and
        // require zero or more confirmations from affected parties
        val action = selectAction(time, location, person = entity, candidates = candidateActions)
        val (performance, pendingConfirms) = startPerformance(
          action = action,
          entity = entity, location = location
        )
        if (DEBUG) println(s"${entity.name} idle,  starting $action will take ${performance.ticksRemaining}")
        //            perform(performance, person = entity)
        val outgoingMessages = pendingConfirms match {
          case None => Mailbox.empty
          case Some(messages) => messages
        }
        (entity.copy(currentActivity = performance), outgoingMessages)

    }
  }

  def generateConfirmationRequests(action: PersonAction,
                                   location: Location,
                                   entity: Person): Option[Outbox] = {
    action match {
      case actions.Farm =>
        val farmToUse = location.facilities.collectFirst { case f: facility.Farm if f.isAvailable => f }
        farmToUse match {
          case None => None
          case Some(farm) =>
            val reservationRequest = Request(
              from = entity.address,
              to = farm.address,
              payload = Reserve
            )
            Some(Mailbox.from(reservationRequest))
        }
      case Metabolize => None
      case TransitionHealth => None
      case PersonNoAction => None
      case Eat => None
      case Sleep => None
    }
  }

  def startPerformance(action: PersonAction, entity: Commoner,
                       location: Location): (CommonerPerformance, Option[Mailbox]) = {
    val confirmationRequests: Option[Mailbox] = generateConfirmationRequests(
      action = action, entity = entity, location = location) match {
      case None => None
      case Some(requests) => Some(requests)
    }

    confirmationRequests match {
      case None =>
        val performance = CommonerPerformance(
          perform = action,
          pendingConfirms = Set[UUID]()
        )
        (performance, None)
      case Some(messages) =>
        val performance = CommonerPerformance(
          perform = action,
          pendingConfirms = messages.map(_.uuid).toSet
        )
        (performance, confirmationRequests)
    }
  }

  def perform(performance: CommonerPerformance, person: Commoner): (Commoner, Outbox) = {
    val progressed = performance.advanceByTickIfConfirmed
    val (nextPerson, nextActivity, outbox) =
      if (progressed.isComplete) {
        if (DEBUG) println(s"$performance complete")
        // TODO handle outbox
        val (updated, outboxFromAction) = initiateAction(progressed.perform, person)
        (updated, Idle, outboxFromAction)
      }
      else (person, progressed, person.outbox)
    (nextPerson.copy(currentActivity = nextActivity), outbox)
  }

  @tailrec
  private def selectAction(datetime: DateTime,
                           location: Location,
                           person: Commoner,
                           candidates: ActionCandidates): PersonAction = {
    candidates match {
      case Nil => PersonNoAction
      case (candidateAction, condition, interactionGenerator) :: remainingCandidates =>
        val shouldAct = condition(datetime, location, person)
        if (DEBUG && shouldAct) {
          val msg = s"Person ${person.name} should perform ${candidateAction.name} at time $datetime"
          println(msg)
        }
        if (shouldAct) candidateAction
        else selectAction(
          datetime, location,
          person,
          remainingCandidates,
        )
    }
  }

  val candidateActions: ActionCandidates = List(

    (Eat, shouldEat, None),
    (Farm, shouldFarm, None), // Some(farmInteraction)),
    //    ("relax", relax, shouldRelax),
    //    ("procreate", procreate, shouldProcreate),
    (Sleep, shouldSleep, None)
  )

  override val involuntaryActions: ReactionCandidates = List(
    (Metabolize, shouldMetabolize),
    (TransitionHealth, shouldTransitionHealth)
  )

  import LocalConfig.DEBUG
  override def initiateAction(action: PersonAction, entity: Commoner): (Commoner, Outbox) = {
    if (DEBUG && action != PersonNoAction) println(action)
    action match {
      case Farm => doFarm(entity)
      case Metabolize => doMetabolize(entity)
      case TransitionHealth => doTransitionHealth(entity)
      case PersonNoAction => (entity, Mailbox.empty)
      case Eat => doEat(entity)
      case Sleep => doSleep(entity)
    }
  }

  private def doSleep(entity: Commoner): (Commoner, Mailbox) = {
    (entity, Mailbox.empty)
  }

  private def doEat(entity: Commoner): (Commoner, Mailbox) = {

    val meal = Meal.cheapestMeal(
      candidateComponents = entity.inventory,
      requiredEnergy = entity.foodEnergyRequired
    )

    meal match {
      case None =>
        if (DEBUG) {
          println(s"No meal could be constructed from ${entity.inventory}")
        }
        (entity, Mailbox.empty)
      case Some(eatenMeal) =>
        val fatGained = eatenMeal.energy / ENERGY_PER_KILO_OF_FAT
        val newBodyFat = entity.availableBodyFat + fatGained
        val newInventory = entity.inventory.deductMeal(eatenMeal)

        if (DEBUG) println(f"Yum meal for ${entity.name} ${eatenMeal.kilocalories}%1.3f new fat $fatGained to reach $newBodyFat." +
          f"Remaining inventory has ${newInventory.totalAvailableCalories}%1.3f")

        val updated = entity.copy(
          inventory = newInventory,
          availableBodyFat = newBodyFat
        )
        (updated, Mailbox.empty)
    }

  }

  private def doFarm(entity: Commoner): (Commoner, Mailbox) = {

    /**
      * People can take advantage of local available arable land to produce a certain quantity of
      * crops and/or live stock.
      **
      * We'd like to be able to select a subset of possible crops ot farm, and then randomly generate a crop yield
      * amount based on a distribution that is specific to each type of crop. Ideally we could create a "yield map"
      * which would map from Type to Yield amount.
      **
      * Ideally farms would have some degree of persistence so that we could model the fact that growing crops inevitably takes
      * time, but maybe that's a future feature.
      *
      */

    val cropToFarm: SimpleFood = SimpleFood.randomCrop()

    val cropYield: Int = cropToFarm.randomYield
    val produce = FoodItemGroup(Map[Freshness, Int](Fresh -> cropYield), sku = cropToFarm)

    val newInventory = addProduceToInventory(produce = produce, inventory = entity.inventory,
      cropToFarm = cropToFarm)
    if (DEBUG) {
      println(s"Yield $produce")
      println(s"old inventory ${entity.name} ${entity.inventory.contents.get(cropToFarm)}; new inventory ${newInventory.contents.get(cropToFarm)}")
    }
    (entity.copy(inventory = newInventory), Mailbox.empty)
  }

  private def doTransitionHealth(entity: Commoner): (Commoner, Mailbox) = {

    val (worseChance, betterChance) = HealthStatus.transitionProbabilities(entity.ageBracket)

    val nextHealth = transitionHealth(worseChance = worseChance,
      betterChance = betterChance,
      starting = entity.health)

    if (nextHealth == Dead) println(s"${entity.name} died in a health event")
    val updated = entity.copy(
      health = nextHealth
    )
    (updated, Mailbox.empty)

  }

  private def doMetabolize(entity: Commoner): (Commoner, Mailbox) = {

    val fatBurned = entity.foodEnergyRequired / ENERGY_PER_KILO_OF_FAT

    val updatedBodyFat = entity.availableBodyFat - fatBurned
    val updatedHealth = if (updatedBodyFat <= Kilograms(0)) {
      if (DEBUG) {
        println(s"${entity.name} died because body fat fell to $updatedBodyFat")
      }
      Dead
    } else entity.health

    if (updatedHealth == Dead) println(s"${entity.name} died from starvation")

    val updated = entity.copy(
      availableBodyFat = updatedBodyFat,
      health = updatedHealth
    )
    (updated, Mailbox.empty)

  }

  override val NoAction: PersonAction = PersonNoAction
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

