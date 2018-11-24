package inventory

import meal.Meal
import resource._

import scala.util.Random

case class Inventory(contents: List[InventoryItem]) {
  def deductMeal(meal: Meal): Inventory = {
    Inventory(contents.diff(meal.ingredients))
  }

  val size: Int = contents.length

  def edibleItems: List[FoodItem] = {
    contents.collect {
      case f: FoodItem => f
    }
  }

  def randomSample(n: Int): Inventory = {
    Inventory(Random.shuffle(contents).take(n))
  }

  def +(newInventory: Inventory): Inventory = {
    Inventory(contents ++ newInventory.contents)
  }

  def +(newItem: InventoryItem): Inventory = {
    Inventory(contents ++ List(newItem))
  }
}

object Inventory {
  def fromManifest(manifest: Map[SKU, Int]): Inventory = {
    val contents = manifest.flatMap { case (sku, amount) =>
      List.fill(amount) {
        sku match {
          case foodType: SimpleFood => FoodItem(sku = foodType, units = amount, freshness = Fresh)
        }
      }
    }.toList
    Inventory(contents)
  }
}