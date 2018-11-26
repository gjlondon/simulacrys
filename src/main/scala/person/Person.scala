package person

import constants.Constants.CALORIES_PER_KILO_OF_FAT
import demographic._
import inventory.FoodInventory
import meal.Meal
import org.joda.time.DateTime
import person.Commoner.{eat, farm, metabolize, party}
import resource.Calorie.calorie
import resource._
import squants.energy.Energy
import squants.mass._
import squants.motion.Distance
import status._

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

sealed trait Person {
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
  def act(time: DateTime): Person
// TODO figure out how to make these covariant so they can be defined on the trait
  //  val eat: Commoner => Commoner
//  val act: (Commoner, DateTime) => Commoner
//  val metabolize: Commoner => Commoner
//  val relax: Commoner => Commoner

  val caloriesRequired: Energy = {
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
}


case class Commoner(name: String, inventory: FoodInventory,
                    age: AgeBracket, gender: Gender, height: Distance,
                    availableBodyFat: Mass, leanBodyMass: Mass,
                    health: HealthStatus = Fine) extends Person {

  def act(time: DateTime): Commoner =  {

    val afterMetabolism = metabolize(this)
    val afterEating = eat(afterMetabolism)

    afterEating.weightStatus match {
      case Obese | Overweight => party(afterEating)
      case Normal | Underweight | DangerouslyLow => farm(afterEating)
    }
  }
}

object Commoner {


  val metabolize: Commoner => Commoner = { person: Commoner =>
    val fatBurned = person.caloriesRequired / CALORIES_PER_KILO_OF_FAT

    person.copy (
      availableBodyFat = person.availableBodyFat - fatBurned,
      health = if (person.availableBodyFat <= Kilograms(0)) Dead else person.health
    )
  }

  //  override def metabolize(): Commoner = {
  //    val fatBurned = caloriesRequired / CALORIES_PER_KILO_OF_FAT
  //
  //    this.copy(
  //      availableBodyFat = availableBodyFat - fatBurned,
  //      health = if (availableBodyFat <= Kilograms(0)) Dead else health
  //    )
  //  }

  val eat: Commoner => Commoner = { person: Commoner =>
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



  val farm: Commoner => Commoner = { person: Commoner =>
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

  val relax: Commoner => Commoner = { c: Commoner => {
    //    println(s"$name is having a good time")
    c
  }}


  val party: Commoner => Commoner = { c: Commoner =>
    relax(c)
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

