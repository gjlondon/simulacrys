package inventory

import meal.Meal
import resource._

import scala.util.Random

case class FoodInventory private(contents: List[FoodItem]) {

  def deductMeal(meal: Meal): FoodInventory = {
    val deductedInventory = contents.diff(meal.ingredients)
    FoodInventory(deductedInventory)
  }

  val size: Int = contents.length

  def randomSample(n: Int): FoodInventory = {
    FoodInventory(Random.shuffle(contents).take(n))
  }

  def +(newInventory: FoodInventory): FoodInventory = {
    FoodInventory(contents ++ newInventory.contents)
  }

  def +(newItem: FoodItem): FoodInventory = {
    FoodInventory(contents ++ List(newItem))
  }
}

object FoodInventory {

  def apply(contents: List[FoodItem]): FoodInventory = {

    val contentList: scala.List[FoodItem] = compactContents(contents)
    //    println(s"""inventory compacted from $origSize to ${contentList.length}""")
    new FoodInventory(contentList)
  }

  private def compactContents(contents: List[FoodItem]): List[FoodItem] = {
    val origSize = contents.size
    val similarItems: Map[(SimpleFood, Freshness), Seq[FoodItem]] = contents.groupBy { item => (item.sku, item.freshness) }
    val compactedContents: Iterable[FoodItem] = similarItems map {
      case ((sku: SimpleFood, freshness: Freshness), items: Seq[FoodItem]) =>
        val totalUnits: Int = items.foldLeft(0) { (total, next) => total + next.units }
        FoodItem(sku = sku, freshness = freshness, units = totalUnits)
    }
    val filteredContents: Iterable[FoodItem] = compactedContents collect { case item if item.units > 0 => item }
    // TODO figure out why the toList call needs to be on a separate line
    filteredContents.toList
  }

  def fromManifest(manifest: Map[SKU, Int]): FoodInventory = {
    val contents = manifest.flatMap { case (sku, amount) =>
      List.fill(amount) {
        sku match {
          case foodType: SimpleFood => FoodItem(sku = foodType, units = amount, freshness = Fresh)
        }
      }
    }.toList
    FoodInventory(contents)
  }
}