package person

import demographic._
import inventory.Inventory
import meal.Meal
import resource._
import squants.energy.Energy
import squants.mass._
import Calorie.calorie
import squants.motion.Distance
import status._
import constants.Constants.CALORIES_PER_KILO_OF_FAT

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

import healthCalculations.{calcBodyMassIndex, calcWeightStatus}


sealed trait Person {
  val name: String
  val inventory: Inventory
  val health: HealthStatus
  val age: AgeBracket
  val gender: Gender
  val height: Distance
  val leanBodyMass: Mass
  val availableBodyFat: Mass

  def weight: Mass = leanBodyMass + availableBodyFat
  def bodyMassIndex: AreaDensity = calcBodyMassIndex(height, weight)
  def weightStatus: WeightStatus = calcWeightStatus(bodyMassIndex)
  def eat(): Person
  def act(): Person
  def metabolize(): Person
  def relax(): Unit



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


case class Commoner(name: String, inventory: Inventory,
                    age: AgeBracket, gender: Gender, height: Distance,
                    availableBodyFat: Mass, leanBodyMass: Mass,
                    health: HealthStatus = Fine) extends Person {

  override def metabolize(): Commoner = {
    val fatBurned = caloriesRequired / CALORIES_PER_KILO_OF_FAT

    this.copy(
      availableBodyFat = availableBodyFat - fatBurned,
      health = if (availableBodyFat <= Kilograms(0)) Dead else health
    )
  }

  override def eat(): Commoner = {
    val meal = candidateMeal(
      fromComponents = inventory,
      requiredCalories = caloriesRequired
    )

    meal match {
      case None => this
      case Some(eatenMeal) =>
        val fatGained = eatenMeal.calories / CALORIES_PER_KILO_OF_FAT
        this.copy(
          inventory = inventory.deductMeal(eatenMeal),
          availableBodyFat = availableBodyFat + fatGained
        )
    }
  }



  def farm(): Commoner = {
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

    val cropYield = cropToFarm.randomYield
    val newInventory = inventory + cropYield
    this.copy(inventory = newInventory)
  }

  def candidateMeal(fromComponents: Inventory, requiredCalories: Energy, size: Int = 1): Option[Meal] = {
    if (size >= fromComponents.size) {
      return None
    }

    val ingredients = fromComponents.randomSample(size)
    val meal = Meal.fromIngredients(ingredients.edibleItems)
    if (meal.calories >= requiredCalories) {
      Some(meal)
    }
    else {
      candidateMeal(
        fromComponents = fromComponents,
        requiredCalories = requiredCalories,
        size = size + 1
      )
    }
  }

  def party(): Commoner = {
    relax()
    this
  }

  override def relax(): Unit = {
//    println(s"$name is having a good time")
  }

  override def act(): Commoner = {
    val afterMetabolism = metabolize()
    val afterEating = afterMetabolism.eat()

    afterEating.weightStatus match {
      case Obese | Overweight => afterEating.party()
      case Normal | Underweight | DangerouslyLow => afterEating.farm()
    }
  }


}

object PersonNames {
  def nextName: String = names(Random.nextInt(names.length))

  val names = Vector(
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

