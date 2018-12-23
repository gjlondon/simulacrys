package inventory

import meal.Meal
import resource._
import squants.Energy
import squants.energy.{Energy, Joules}

case class FoodInventory private (contents: Map[SimpleFood, FoodItemGroup]) {

  def deductMeal(meal: Meal): FoodInventory = {
//    println(s"before $contents")

//    println(s"ingredients ${meal.ingredients}")
    val ingredients: Map[SimpleFood, FoodItemGroup] = meal.ingredients

    // TODO find a more efficient way to do this. Can it be expressed as
    // a direct subtraction on inventories?
    val deductedInventory = contents map {
      case (food: SimpleFood, group: FoodItemGroup) =>
        ingredients.get(food) match {
          case None => food -> group
          case Some(consumed: FoodItemGroup) => food -> (group - consumed)
        }
    }
//    println(s"after $deductedInventory")
    FoodInventory(deductedInventory)
  }

  val isEmpty: Boolean = contents.isEmpty
  val size: Int = contents.size
  def totalAvailableFoodEnergy: Energy = {
    FoodInventory.energyInIngredients(contents)
  }

  def totalAvailableCalories: Double = {
    import resource.Calorie.calorie
    totalAvailableFoodEnergy / calorie
  }

  def cheapestComponent: FoodItemGroup = {
    contents.values.toList.sortWith { case (a: FoodItemGroup, b: FoodItemGroup) => a.size  > b.size }.head
  }

  def +(newInventory: FoodInventory): FoodInventory = {
    FoodInventory(contents ++ newInventory.contents)
  }
}

object FoodInventory {

  def apply(contents: Map[SimpleFood, FoodItemGroup]): FoodInventory = {
    val filteredContents: Map[SimpleFood, FoodItemGroup] = contents collect {
      case (sku, group) if group.size > 0 => sku -> group
    }
    new FoodInventory(filteredContents)
  }

  def energyInIngredients(ingredients: Map[SimpleFood, FoodItemGroup]): Energy = {
    ingredients.foldLeft(Joules(0)) { case (total: Energy, (foodType: SimpleFood, ingredientGroup: FoodItemGroup)) =>
      val energyPerKg = foodType.energyPerKg
      val unitWeight = foodType.unitWeight
      val caloriesInGroup = ingredientGroup.contents.foldLeft(Joules(0)) { case (groupTotal: Energy, (_, quantity: Int)) =>
        val energyFromIngredients = energyPerKg * unitWeight * quantity
        groupTotal + energyFromIngredients
      }
      caloriesInGroup
    }
  }
}