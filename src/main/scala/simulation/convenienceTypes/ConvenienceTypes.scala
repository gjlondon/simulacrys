package simulation.convenienceTypes

import simulation.location.Location
import simulation.resource.{Commodity, SimpleFood}
import squants.mass.Mass

object ConvenienceTypes {
  // intended to represent a 2D grid, but I realized that indexing
  // tricks can make it appear 2D, while actually storing in 1D makes mapping etc easier
  type VectorGrid = Vector[Location]
  type CommodityMap = Map[Commodity, Mass]
  type FoodMap = Map[SimpleFood, Mass]
}
