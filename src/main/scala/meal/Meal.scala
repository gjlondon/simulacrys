package meal

import resource._
import squants.energy.{Energy, Joules}

case class Meal private (calories: Energy, ingredients: List[FoodItem])

object Meal {

  def fromIngredients(ingredients: List[FoodItem]): Meal = {
    // TODO don't consume full stack on each meal
    val totalCalories = ingredients.foldLeft(Joules(0)) { (total, ingredient) =>
      val foodType: SimpleFood = ingredient.sku
      total + foodType.caloriesPerKg * foodType.unitWeight * ingredient.units
    }

    Meal(calories = totalCalories, ingredients = ingredients)
  }
}