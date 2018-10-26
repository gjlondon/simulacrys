package meal

import inventory.Inventory
import resource.{Carbs, Fat, MacroNutrient, Protein}

case class Meal(unitsOfProtein: Int = 0,
                unitsOfFat: Int = 0,
                unitsOfCarbs: Int = 0) {
  val totalCalories: Int = {
    Protein.caloriesPerUnit * unitsOfProtein +
      Fat.caloriesPerUnit * unitsOfFat +
      Carbs.caloriesPerUnit * unitsOfCarbs
  }

  def asIngredients: List[MacroNutrient] = {
    val fat: List[MacroNutrient] = List.fill(unitsOfFat)(Fat)
    val carbs: List[MacroNutrient] = List.fill(unitsOfCarbs)(Carbs)
    val protein: List[MacroNutrient] = List.fill(unitsOfProtein)(Protein)
    fat ++ carbs ++ protein
  }
}

object Meal {

  def fromIngredients(ingredients: Inventory): Meal = {
    val itemCounts = ingredients.itemCounts

    val unitsOfProtein = itemCounts.getOrElse(Protein, 0)
    val unitsOfFat = itemCounts.getOrElse(Fat, 0)
    val unitsOfCarbs = itemCounts.getOrElse(Carbs, 0)

    Meal(
      unitsOfCarbs = unitsOfCarbs,
      unitsOfFat = unitsOfFat,
      unitsOfProtein = unitsOfProtein
    )
  }


}