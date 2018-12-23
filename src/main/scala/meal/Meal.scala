package meal

import inventory.FoodInventory
import resource._
import squants.energy.{Energy, Joules}

case class Meal private (energy: Energy, ingredients: Map[SimpleFood, FoodItemGroup]) {

  import resource.Calorie.calorie

  def kilocalories: Double = {
    energy / calorie
  }
}

object Meal {

  def fromIngredients(ingredients: Map[SimpleFood, FoodItemGroup]): Meal = {
    Meal(
      energy = FoodInventory.energyInIngredients(ingredients),
      ingredients = ingredients
    )
  }

  def cheapestMeal(candidateComponents: FoodInventory,
                   selectedComponents: FoodInventory = FoodInventory(Map[SimpleFood, FoodItemGroup]()),
                   requiredEnergy: Energy): Option[Meal] = {
    // We're out of ingredients
    if (candidateComponents.isEmpty) { return None }

    val energySoFar: Energy = FoodInventory.energyInIngredients(selectedComponents.contents)
    val energyDeficit = requiredEnergy - energySoFar

    // we need to add more ingredients to get enough calories
    val cheapestFood: FoodItemGroup = candidateComponents.cheapestComponent
    val foodType: SimpleFood = cheapestFood.sku
    val requiredUnitsToCoverDeficit = Math.ceil(
      energyDeficit / (foodType.energyPerKg * foodType.unitWeight)
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
      requiredEnergy = requiredEnergy
    )
  }
}