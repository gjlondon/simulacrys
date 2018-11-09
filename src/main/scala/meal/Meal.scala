package meal

import resource._
import squants.energy.{Energy, Joules}

case class Meal private (calories: Energy, ingredients: List[SimpleFood])

object Meal {

  def fromIngredients(ingredients: List[SimpleFood]): Meal = {
    val totalCalories = ingredients.foldLeft(Joules(0)) { (total, ingredient) =>
      total + ingredient.caloriesPerKg * ingredient.unitWeight
    }

    Meal(calories = totalCalories, ingredients = ingredients)
  }
}