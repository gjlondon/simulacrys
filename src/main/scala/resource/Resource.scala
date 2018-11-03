package resource

import squants.Quantity
import squants.energy.{Energy, Kilojoules}
import squants.mass.{Kilograms, Mass}

import scala.util.Random



sealed trait Resource {
  // val name: String

}

sealed trait QuantifiableResource[Q] {
  val amount: Quantity[Q]
}
sealed trait InventoryItem[Q] extends QuantifiableResource[Q]

sealed trait Commodity[Q] extends QuantifiableResource[Q]

sealed trait MacroNutrient extends QuantifiableResource[Quantity[Energy]] {
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
  override val caloriesPerUnit = 1000
  override val amount: Energy = Kilojoules(10)
}
case object Protein extends MacroNutrient {
  override val caloriesPerUnit: Int = 500
  override val amount: Energy = Kilojoules(10)

}
case object Carbs extends MacroNutrient {
  override val caloriesPerUnit: Int = 800
  override val amount: Energy = Kilojoules(10)

}

sealed trait Food extends QuantifiableResource[Mass]

// vegetable products
case class cereals(amount: Mass) extends Food
case class wheat(amount: Mass) extends Food
case class rice(amount: Mass) extends Food
case class barley(amount: Mass) extends Food
case class maize(amount: Mass) extends Food
case class rye(amount: Mass) extends Food
case class oats(amount: Mass) extends Food
case class millet(amount: Mass) extends Food
case class sorghum(amount: Mass) extends Food

// starchyRoots
case class potatoes(amount: Mass) extends Food
case class sweetPotatoes(amount: Mass) extends Food
case class cassava(amount: Mass) extends Food
case class yams(amount: Mass) extends Food
case class otherRoots(amount: Mass) extends Food

// sugarCrops
case class sugarCane(amount: Mass) extends Food
case class sugarBeet(amount: Mass) extends Food

// sweeteners
case class sugar(amount: Mass) extends Food
case class honey(amount: Mass) extends Food

// pulses
case class beans(amount: Mass) extends Food
case class peas(amount: Mass) extends Food

// treeNuts
case class treeNuts(amount: Mass) extends Food

// oilcrops
case class soybeans(amount: Mass) extends Food
case class groundNuts(amount: Mass) extends Food
case class sunflowerSeed(amount: Mass) extends Food
case class rapeAndMustardSeed(amount: Mass) extends Food
case class cottonseed(amount: Mass) extends Food
case class coconuts(amount: Mass) extends Food
case class sesameSeed(amount: Mass) extends Food
case class palmKernels(amount: Mass) extends Food
case class olives(amount: Mass) extends Food

// oils
case class vegetableOils(amount: Mass) extends Food
case class soybeanOil(amount: Mass) extends Food
case class groundnutOil(amount: Mass) extends Food
case class sunflowerSeedOil(amount: Mass) extends Food
case class rapeAndMustardOil(amount: Mass) extends Food
case class cottonseedOil(amount: Mass) extends Food
case class palmKernelOil(amount: Mass) extends Food
case class palmOil(amount: Mass) extends Food
case class coconutOil(amount: Mass) extends Food
case class sesameSeedOil(amount: Mass) extends Food
case class oliveOil(amount: Mass) extends Food
case class riceBranOil(amount: Mass) extends Food
case class maizeGermOil(amount: Mass) extends Food

// vegetables
case class tomatoes(amount: Mass) extends Food
case class onions(amount: Mass) extends Food

// fruit
case class orangesAndMandarins(amount: Mass) extends Food
case class lemonsAndLimes(amount: Mass) extends Food
case class grapefruit(amount: Mass) extends Food
case class bananas(amount: Mass) extends Food
case class plantains(amount: Mass) extends Food
case class apples(amount: Mass) extends Food
case class pineapples(amount: Mass) extends Food
case class dates(amount: Mass) extends Food
case class grapes(amount: Mass) extends Food

// stimulants
case class coffee(amount: Mass) extends Food
case class cocoaBeans(amount: Mass) extends Food
case class tea(amount: Mass) extends Food

// spices
case class pepper(amount: Mass) extends Food
case class pimento(amount: Mass) extends Food
case class cloves(amount: Mass) extends Food

// alcoholicBeverages
case class wine(amount: Mass) extends Food
case class barleyBeer(amount: Mass) extends Food
case class liquor(amount: Mass) extends Food

// animalProducts
case class bovineMeat(amount: Mass) extends Food
case class mutton(amount: Mass) extends Food
case class goatMeat(amount: Mass) extends Food
case class pigMeat(amount: Mass) extends Food
case class poultryMeat(amount: Mass) extends Food
case class offals(amount: Mass) extends Food

// animalFats
case class butter(amount: Mass) extends Food
case class cream(amount: Mass) extends Food
case class milk(amount: Mass) extends Food
case class eggs(amount: Mass) extends Food

// fishAndSeaFood
case class freshwaterFish(amount: Mass) extends Food
case class demersalFish(amount: Mass) extends Food
case class pelagicFish(amount: Mass) extends Food
case class marineFish(amount: Mass) extends Food
case class crustaceans(amount: Mass) extends Food
case class cephalopods(amount: Mass) extends Food
case class aquaticPlants(amount: Mass) extends Food
