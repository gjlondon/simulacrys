package meal

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
    ingredients.foldLeft(Joules(0)) { (total, ingredient) =>
      val foodType: SimpleFood = ingredient.sku
      total + foodType.caloriesPerKg * foodType.unitWeight * ingredient.units
    }
  }
}