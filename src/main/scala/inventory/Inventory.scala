package inventory

import meal.Meal
import resource.{MacroNutrient, Resource}

import scala.util.Random

case class Inventory(contents: List[Resource]) {
  def deductMeal(meal: Meal): Inventory = {
    Inventory(contents.diff(meal.asIngredients))
  }

  val size: Int = contents.length

  def edibleItems: Inventory = {
    val nutrients = contents.collect { case m: MacroNutrient => m }
    Inventory(nutrients)
  }

  def randomSample(n: Int): Inventory = {
    Inventory(Random.shuffle(contents).take(n))
  }

  def itemCounts: Map[Resource, Int] = {
    contents.groupBy(identity).mapValues(_.size)
  }
}

object Inventory {
  def fromManifest(manifest: Map[Resource, Int]): Inventory = {
    val contents = manifest.flatMap { case (key, value) => List.fill(value)(key) }.toList
    Inventory(contents)
  }
}