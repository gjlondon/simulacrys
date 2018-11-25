package inventory

import meal.Meal
import resource._

import scala.util.Random

case class FoodInventory(contents: Map[SimpleFood, FoodItemGroup]) {

  def deductMeal(meal: Meal): FoodInventory = {
    println(s"before $contents")

    println(s"ingredients ${meal.ingredients}")
    val ingredients: Map[SimpleFood, FoodItemGroup] = meal.ingredients

    val deductedInventory = contents map {
      case (food: SimpleFood, group: FoodItemGroup) =>
        ingredients.get(food) match {
          case None => food -> group
          case Some(consumed: FoodItemGroup) => food -> (group - consumed)
        }
    }
    println(s"after $deductedInventory")
    FoodInventory(deductedInventory)
  }

  val size: Int = contents.size
//
//  def randomSample(n: Int): FoodInventory = {
//    FoodInventory(Random.shuffle(contents).take(n))
//  }

  def +(newInventory: FoodInventory): FoodInventory = {
    FoodInventory(contents ++ newInventory.contents)
  }

//  def +(newItem: FoodItemGroup): FoodInventory = {
//    FoodInventory(contents ++ List(newItem))
//  }
}

object FoodInventory {
//
//  def apply(contents: Map[SimpleFood, FoodItemGroup]): FoodInventory = {
//
//    val contentList: Map[SimpleFood, FoodItemGroup] = compactContents(contents)
//    //    println(s"""inventory compacted from $origSize to ${contentList.length}""")
//    new FoodInventory(contentList)
//  }

//  private def compactContents(contents: List[FoodItemGroup]): Map[SKU, FoodItemGroup] = {
//    val origSize = contents.size
//    val similarItems: Map[SimpleFood, Seq[FoodItemGroup]] = contents.groupBy { item => item.sku }
////    for {
////      (sku, itemGroups) <- similarItems
////    } yield {
////      itemGroup.contents map {
////        case (quality, quantity) =>
////          val totalUnits: Int = items.foldLeft(0) { (total, next) => total + next.units }
////          sku -> FoodItemGroup(sku = sku, freshness = freshness, units = totalUnits)
////      }
////    }
//    val compactedContents: Map[SKU, FoodItemGroup] = similarItems map {
//      case (sku: SimpleFood, items: Seq[FoodItemGroup]) =>
//        val compactGroup = items.foldLeft(FoodItemGroup()) { (total: Map[Freshness, Int], next: Map[Freshness, Int]) =>
//          next.contents map {
//            case (thisQuality, thisQuantity) =>
//              thisQuality -> total.getOrElse(thisQuality, 0) + thisQuantity
//          }
//        }
//        sku -> compactGroup
//    }
//    val filteredContents: Map[SKU, FoodItemGroup] = compactedContents collect {
//      case (sku, item) if item.units > 0 => sku -> item
//    }
//    // TODO figure out why the toList call needs to be on a separate line
//    filteredContents
//  }

  def fromManifest(manifest: Map[SKU, Int]): FoodInventory = {
    val contents = manifest.flatMap { case (sku, amount) =>
      List.fill(amount) {
        sku match {
          case foodType: SimpleFood => FoodItemGroup(sku = foodType, units = amount, freshness = Fresh)
        }
      }
    }.toList
    FoodInventory(contents)
  }
}