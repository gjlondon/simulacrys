package person

import demographic._
import inventory.Inventory
import meal.Meal
import resource._
import squants.energy.Energy
import squants.mass.Kilograms
import Calorie.calorie
import status._

import scala.util.Random

sealed trait Person {
  val name: String
  val inventory: Inventory
  val health: HealthStatus
  val age: AgeBracket
  val gender: Gender
  def eat(): Person
  def act(): Person
  def relax(): Unit

  val caloriesRequired: Energy = {
    val required: Int = (age, gender) match {
      case (Child, Female) => 1500
      case (Child, Male) => 1600
      case (Young, Female) => 1700
      case (Young, Male) => 1800
      case (Adult, Female) => 1900
      case (Adult, Male) => 2000
      case (Old, Female) => 1800
      case (Old, Male) => 1900
    }

    required * calorie
  }
}


case class Commoner(name: String, inventory: Inventory,
                    age: AgeBracket, gender: Gender,
                    health: HealthStatus = Fine) extends Person {
  override def eat(): Commoner = {
    val meal = candidateMeal(
      fromComponents = inventory,
      requiredCalories = caloriesRequired
    )

    meal match {
      case None => this.copy(health = health.nextWorst)
      case Some(eatenMeal) => this.copy(
        inventory = inventory.deductMeal(eatenMeal),
        health = health.nextBest
      )
    }
  }

  def farm(): Commoner = {
    println(s"$name is farming")
    val produce = Inventory(List(Beans(Kilograms(1)), Meat(Kilograms(1))))
    val newInventory = inventory + produce
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
    println(s"$name is having a good time")
  }

  override def act(): Commoner = {
    val afterEating = eat()

    afterEating.health match {
      case Robust | Fine => afterEating.party()
      case Sick | Poor => afterEating.farm()
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