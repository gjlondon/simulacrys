package world

import location.{City, Farm, Location}
import populace.Populace

import scala.util.Random

object LocationTypes {
  // intended to represent a 2D grid, but I realized that indexing
  // tricks can make it appear 2D, while actually storing in 1D makes mapping etc easier
  type VectorGrid = Vector[Location]
}

import LocationTypes.VectorGrid

class World private (startingGrid: Grid) {
  val grid: Grid = startingGrid

  def fullPopulace: Populace = {
    grid.fullPopulace
  }

}

object World {
  def farmWorld: World = new World(startingGrid = Grid.allFarms)
  def cityWorld: World = new World(startingGrid = Grid.allCities)
  def fromGrid(grid: Grid): World = new World(startingGrid = grid)
  def fromLocations(locations: VectorGrid): World = new World(Grid(locations))
  def randomWorld: World = if (Random.nextInt(1) == 1) World.farmWorld else World.cityWorld
}

class Grid private (val positions: VectorGrid) {
  def fullPopulace: Populace = {
    positions.foldLeft(Populace.empty) { (a, loc) =>
      a ++ loc.populace
    }
  }
}

object Grid {

  def apply(positions: VectorGrid): Grid = {
    new Grid(positions = positions)
  }

  def fillGrid[T <: Location](withElem: => T, size: Int = 10): Grid = {
    val positions: VectorGrid = Vector.fill[T](size * size)(withElem)
    Grid(positions)
  }

  def allFarms: Grid = {
    fillGrid {
      val farmName = Random.nextString(5)
      Farm(farmName, Populace.randomPop(15))
    }
  }

  def allCities: Grid = {
    fillGrid {
      val cityName = Random.nextString(5)
      City(cityName, Populace.randomPop(25))
    }
  }
}