package meal

import resource._
import squants.energy.{Energy, Joules}

case class Meal private (calories: Energy, ingredients: List[FoodItem])

object Meal {

  def fromIngredients(ingredients: List[FoodItem]): Meal = {
    val totalCalories = ingredients.foldLeft(Joules(0)) { (total, ingredient) =>
      val foodType: SimpleFood = ingredient.sku
      total + foodType.caloriesPerKg * foodType.unitWeight
    }

    Meal(calories = totalCalories, ingredients = ingredients)
  }
}