package inventory

import convenienceTypes.ConvenienceTypes.CommodityMap
import meal.Meal
import resource.{Commodity, SimpleFood}
import squants.mass.{Kilograms, Mass}

import scala.util.Random

sealed trait Inventory


// TODO can this directly extend map?
case class CommodityInventory(contents: CommodityMap)
{
  def deductMeal(meal: Meal): CommodityInventory = {
    val newContents = contents.map {
      case (k: SimpleFood, v: Mass) => k -> (v - meal.ingredients.getOrElse(k,Kilograms(0)))
      case (k: Commodity, v: Mass) => k -> v
    }
    CommodityInventory(contents = newContents)
  }

  val size: Int = contents.size

  def edibleItems: CommodityMap = {
    contents.filterKeys { case _: SimpleFood => true }
  }

  def weightedRandomElement(): Commodity = {
    val weightRanges = contents.toSeq.sortBy(_._2)
    val weights = weightRanges map { item => item._2 / size}
    val roll = Random.nextFloat()
    val position = weights.indexWhere( weight => roll < weight.value )
    weightRanges(position)._1
  }

//  def itemCounts: Map[Commodity, Int] = {
//    contents.groupBy(identity).mapValues(_.size)
//  }

  def +(newInventory: CommodityInventory): CommodityInventory = {
    // TODO replace with semigroup?
    val newContents = contents ++ newInventory.contents.map{
      case (k: Commodity,v: Mass) => k -> (v + contents.getOrElse(k,Kilograms(0))) }
    CommodityInventory(contents = newContents)
  }

  def -(newInventory: CommodityInventory): CommodityInventory = {
    // TODO replace with semigroup?
    val newContents = newInventory.contents ++ contents.map{
      case (k: Commodity,v: Mass) => k -> (v - newInventory.contents.getOrElse(k,Kilograms(0))) }
    CommodityInventory(contents = newContents)
  }
}

object CommodityInventory {
//  def fromManifest(manifest: Map[Commodity, Int]): CommodityInventory = {
//    val contents = manifest.flatMap { case (key, value) => List.fill(value)(key) }.toList
//    CommodityInventory(contents)
//  }
}