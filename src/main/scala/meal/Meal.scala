package meal

import convenienceTypes.ConvenienceTypes.{CommodityMap, FoodMap}
import resource._
import squants.energy.{Energy, Joules}
import squants.mass.Mass

case class Meal private (calories: Energy, ingredients: FoodMap)

object Meal {

  def fromIngredients(ingredients: FoodMap): Meal = {
    val totalCalories = ingredients.foldLeft(Joules(0)) { case (total, (ingredient: SimpleFood, amount: Mass)) =>
      total + ingredient.caloriesPerKg * amount
    }

    Meal(calories = totalCalories, ingredients = ingredients)
  }
}