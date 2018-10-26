package resource

import scala.util.Random

sealed trait Resource {
  val name: String
}

sealed trait MacroNutrient extends Resource {
  val caloriesPerUnit: Int

  def getRandomNutrient: MacroNutrient = {
    Random.nextInt(3) match {
      case 0 => Fat
      case 1 => Protein
      case 2 => Carbs
    }
  }
}



case object Fat extends MacroNutrient {
  override val name: String = "Fat"
  override val caloriesPerUnit = 1000
}
case object Protein extends MacroNutrient {
  override val name: String = "Protein"
  override val caloriesPerUnit: Int = 500
}
case object Carbs extends MacroNutrient {
  override val name: String = "Carbohydrates"
  override val caloriesPerUnit: Int = 800
}