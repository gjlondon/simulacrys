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
}