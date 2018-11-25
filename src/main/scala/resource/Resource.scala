package resource

import squants.energy.{Energy, Kilojoules, SpecificEnergy}
import squants.mass.{Grams, Kilograms, Mass}

import scala.annotation.tailrec
import scala.util.Random

object Calorie {
  val calorie: Energy = Kilojoules(4.18)
}

import resource.Calorie.calorie

sealed trait Artifact {}

sealed trait SKU {
  val unitWeight: Mass
}

sealed trait Commodity extends SKU

sealed trait Property

sealed trait Endeavor

case object Farming extends Endeavor

sealed trait ItemGroup[T <: Quality]{
  val contents: Map[T, Int]
  val sku: SKU
  val size: Int = contents.values.sum

  protected def mergeContents(rightGroup: ItemGroup[T], op: (Int, Int) => Int): Map[T, Int] = {
    val leftContents = this.contents
    val rightContents = rightGroup.contents
    val allKeys = leftContents.keySet ++ rightContents.keySet
    allKeys.foldLeft(Map[T, Int]()) { (existing: Map[T, Int], nextKey) =>
      val leftVal = leftContents.getOrElse(nextKey, 0)
      val rightVal = rightContents.getOrElse(nextKey, 0)
      existing.updated(nextKey, op(leftVal, rightVal))
    }
  }
}

case class FoodItemGroup private (contents: Map[Freshness, Int], sku: SimpleFood) extends ItemGroup[Freshness] {

  def +(rhs: FoodItemGroup): FoodItemGroup = {
    val mergedContents = mergeContents(rhs, op = { _ + _ })
    FoodItemGroup(contents=mergedContents, sku=sku)
  }

  def -(rhs: FoodItemGroup): FoodItemGroup = {
    val mergedContents = mergeContents(rhs, op = { _ - _ })
    FoodItemGroup(contents=mergedContents, sku=sku)
  }

  def collectCheapestUnits(unitsToConsume: Int): FoodItemGroup = {

    val valueSortedContents = this.contents.toList.sortWith { _._2 > _._2 }

    @tailrec
    def go(unitsToConsume: Int,
           itemsToConsider: FoodItemGroup,
           cheapestUnits: FoodItemGroup): FoodItemGroup = {
      val (freshnessOfCheapest, cheapestQuantity) = valueSortedContents.head

      if (cheapestQuantity >= unitsToConsume) {
        val contentsToDeduct = Map[Freshness, Int](freshnessOfCheapest -> unitsToConsume)
        cheapestUnits + FoodItemGroup(contents = contentsToDeduct, sku=sku)
      }
      else {
        val contentsToDeduct = Map[Freshness, Int](freshnessOfCheapest -> cheapestQuantity)
        val newCheapestUnits = cheapestUnits + FoodItemGroup(contents = contentsToDeduct, sku = sku)
        val remainingItems = itemsToConsider - newCheapestUnits
        go(unitsToConsume - cheapestQuantity, remainingItems, newCheapestUnits)
      }
    }
    go(unitsToConsume, this, FoodItemGroup(Map[Freshness, Int](), sku))
  }
}

object FoodItemGroup {
  def randomAmountOf(food: SimpleFood, max: Int = 20): FoodItemGroup = {
    val contents = Map[Freshness, Int](
      Fresh -> Random.nextInt(max / 2),
      Old -> Random.nextInt(max / 2),
    )
    FoodItemGroup(contents = contents, sku=food)
  }

  def apply(contents: Map[Freshness, Int], sku: SimpleFood): FoodItemGroup = {
    val filteredContents: Map[Freshness, Int] = contents collect {
      case (quality, quantity) if quantity > 0 => quality -> quantity
    }
    new FoodItemGroup(filteredContents, sku=sku)
  }
}

case class ToolItemGroup(contents: Map[Durability, Int], sku: Tool) extends ItemGroup[Durability] {

  def +(rhs: ToolItemGroup): ToolItemGroup = {
    val mergedContents = mergeContents(rhs, op = { _ + _ })
    ToolItemGroup(contents=mergedContents, sku=sku)
  }


  def -(rhs: ToolItemGroup): ToolItemGroup = {
    val mergedContents = mergeContents(rhs, op = { _ - _ })
    ToolItemGroup(contents=mergedContents, sku=sku)
  }
}

sealed trait Tool extends SKU {
  val usedFor: Endeavor
  val durability: Durability
}

case class FarmingTools(durability: Durability) extends Tool {
  override val usedFor: Endeavor = Farming
  override val unitWeight: Mass = Kilograms(5)
}

sealed trait SimpleFood extends Commodity {
  val caloriesPerKg: SpecificEnergy
  val yieldMean: Int  // units in average harvest
  val yieldStd: Int // variance of harvest

  def randomYield: Int = {
    val surplus = Math.floor(yieldStd * Random.nextGaussian())
    val cropYield = Math.floor(Math.max(yieldMean + surplus, 0)).toInt
    cropYield
  }
}

object SimpleFood {
  def randomCrop(): SimpleFood = {
    val roll = Random.nextInt(100)
    roll match {
      case x if x < 30 => Meat
      case _ => Beans
    }
  }
}


case object Beans extends SimpleFood {
  val caloriesPerKg: SpecificEnergy = (1650 * calorie) / Kilograms(1)
  override val unitWeight: Mass = Kilograms(.25)
  override val yieldMean: Int = 5
  override val yieldStd: Int = 3
}

case object Meat extends SimpleFood {
  val caloriesPerKg: SpecificEnergy = (2500 * calorie) / Kilograms(1)
  override val unitWeight: Mass = Grams(100)
  override val yieldMean: Int = 4
  override val yieldStd: Int = 2
}

sealed trait MacroNutrient extends Property{
  val caloriesPerUnit: Energy

  def getRandomNutrient: MacroNutrient = {
    Random.nextInt(3) match {
      case 0 => Fat
      case 1 => Protein
      case 2 => Carbs
    }
  }
}



case object Fat extends MacroNutrient {
  override val caloriesPerUnit: Energy = Kilojoules(1000)
}

case object Protein extends MacroNutrient {
  override val caloriesPerUnit: Energy = Kilojoules(500)

}
case object Carbs extends MacroNutrient {
  override val caloriesPerUnit: Energy = Kilojoules(800)
}



//sealed trait Food extends Commodity
//
//// vegetable products
//case class cereals(amount: Mass) extends Food
//case class wheat(amount: Mass) extends Food
//case class rice(amount: Mass) extends Food
//case class barley(amount: Mass) extends Food
//case class maize(amount: Mass) extends Food
//case class rye(amount: Mass) extends Food
//case class oats(amount: Mass) extends Food
//case class millet(amount: Mass) extends Food
//case class sorghum(amount: Mass) extends Food
//
//// starchyRoots
//case class potatoes(amount: Mass) extends Food
//case class sweetPotatoes(amount: Mass) extends Food
//case class cassava(amount: Mass) extends Food
//case class yams(amount: Mass) extends Food
//case class otherRoots(amount: Mass) extends Food
//
//// sugarCrops
//case class sugarCane(amount: Mass) extends Food
//case class sugarBeet(amount: Mass) extends Food
//
//// sweeteners
//case class sugar(amount: Mass) extends Food
//case class honey(amount: Mass) extends Food
//
//// pulses
//case class beans(amount: Mass) extends Food
//case class peas(amount: Mass) extends Food
//
//// treeNuts
//case class treeNuts(amount: Mass) extends Food
//
//// oilcrops
//case class soybeans(amount: Mass) extends Food
//case class groundNuts(amount: Mass) extends Food
//case class sunflowerSeed(amount: Mass) extends Food
//case class rapeAndMustardSeed(amount: Mass) extends Food
//case class cottonseed(amount: Mass) extends Food
//case class coconuts(amount: Mass) extends Food
//case class sesameSeed(amount: Mass) extends Food
//case class palmKernels(amount: Mass) extends Food
//case class olives(amount: Mass) extends Food
//
//// oils
//case class vegetableOils(amount: Mass) extends Food
//case class soybeanOil(amount: Mass) extends Food
//case class groundnutOil(amount: Mass) extends Food
//case class sunflowerSeedOil(amount: Mass) extends Food
//case class rapeAndMustardOil(amount: Mass) extends Food
//case class cottonseedOil(amount: Mass) extends Food
//case class palmKernelOil(amount: Mass) extends Food
//case class palmOil(amount: Mass) extends Food
//case class coconutOil(amount: Mass) extends Food
//case class sesameSeedOil(amount: Mass) extends Food
//case class oliveOil(amount: Mass) extends Food
//case class riceBranOil(amount: Mass) extends Food
//case class maizeGermOil(amount: Mass) extends Food
//
//// vegetables
//case class tomatoes(amount: Mass) extends Food
//case class onions(amount: Mass) extends Food
//
//// fruit
//case class orangesAndMandarins(amount: Mass) extends Food
//case class lemonsAndLimes(amount: Mass) extends Food
//case class grapefruit(amount: Mass) extends Food
//case class bananas(amount: Mass) extends Food
//case class plantains(amount: Mass) extends Food
//case class apples(amount: Mass) extends Food
//case class pineapples(amount: Mass) extends Food
//case class dates(amount: Mass) extends Food
//case class grapes(amount: Mass) extends Food
//
//// stimulants
//case class coffee(amount: Mass) extends Food
//case class cocoaBeans(amount: Mass) extends Food
//case class tea(amount: Mass) extends Food
//
//// spices
//case class pepper(amount: Mass) extends Food
//case class pimento(amount: Mass) extends Food
//case class cloves(amount: Mass) extends Food
//
//// alcoholicBeverages
//case class wine(amount: Mass) extends Food
//case class barleyBeer(amount: Mass) extends Food
//case class liquor(amount: Mass) extends Food
//
//// animalProducts
//case class bovineMeat(amount: Mass) extends Food
//case class mutton(amount: Mass) extends Food
//case class goatMeat(amount: Mass) extends Food
//case class pigMeat(amount: Mass) extends Food
//case class poultryMeat(amount: Mass) extends Food
//case class offals(amount: Mass) extends Food
//
//// animalFats
//case class butter(amount: Mass) extends Food
//case class cream(amount: Mass) extends Food
//case class milk(amount: Mass) extends Food
//case class eggs(amount: Mass) extends Food
//
//// fishAndSeaFood
//case class freshwaterFish(amount: Mass) extends Food
//case class demersalFish(amount: Mass) extends Food
//case class pelagicFish(amount: Mass) extends Food
//case class marineFish(amount: Mass) extends Food
//case class crustaceans(amount: Mass) extends Food
//case class cephalopods(amount: Mass) extends Food
//case class aquaticPlants(amount: Mass) extends Food
