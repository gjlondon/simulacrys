package meal

import inventory.FoodInventory
import resource._
import squants.energy.{Energy, Joules}

case class Meal private (calories: Energy, ingredients: Map[SimpleFood, FoodItemGroup])

object Meal {

  def fromIngredients(ingredients: Map[SimpleFood, FoodItemGroup]): Meal = {
    Meal(
      calories = caloriesInIngredients(ingredients),
      ingredients = ingredients
    )
  }

  def caloriesInIngredients(ingredients: Map[SimpleFood, FoodItemGroup]): Energy = {
    ingredients.foldLeft(Joules(0)) { case (total: Energy, (foodType: SimpleFood, ingredientGroup: FoodItemGroup)) =>
      val caloriesPerKg = foodType.caloriesPerKg
      val unitWeight = foodType.unitWeight
      val caloriesInGroup = ingredientGroup.contents.foldLeft(Joules(0)) { case (groupTotal: Energy, (_, quantity: Int)) =>
        val caloriesFromIngredient = caloriesPerKg * unitWeight * quantity
        groupTotal + caloriesFromIngredient

      }
      caloriesInGroup
    }
  }

  def cheapestMeal(candidateComponents: FoodInventory,
                   selectedComponents: FoodInventory = FoodInventory(Map[SimpleFood, FoodItemGroup]()),
                   requiredCalories: Energy): Option[Meal] = {
    // We're out of ingredients
    if (candidateComponents.isEmpty) { return None }

    val caloriesSoFar: Energy = Meal.caloriesInIngredients(selectedComponents.contents)
    val calorieDeficit = requiredCalories - caloriesSoFar

    // we need to add more ingredients to get enough calories
    val cheapestFood: FoodItemGroup = candidateComponents.cheapestComponent
    val foodType: SimpleFood = cheapestFood.sku
    val requiredUnitsToCoverDeficit = Math.ceil(
      calorieDeficit / (foodType.caloriesPerKg * foodType.unitWeight)
    ).toInt

    if (cheapestFood.size >= requiredUnitsToCoverDeficit) {
      val cheapestIngredients = cheapestFood.collectCheapestUnits(requiredUnitsToCoverDeficit)
      val finalIngredientsForMeal = selectedComponents.contents.updated(foodType, cheapestIngredients)
      return Some(Meal.fromIngredients(finalIngredientsForMeal))
    }

    val consumedContents = selectedComponents.contents.updated(foodType, cheapestFood)
    val remainingContents = candidateComponents.contents - foodType
    cheapestMeal(
      candidateComponents = FoodInventory(contents = remainingContents),
      selectedComponents = FoodInventory(contents = consumedContents),
      requiredCalories = requiredCalories
    )
  }
}