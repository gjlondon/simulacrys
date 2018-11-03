package world

import location.{City, Farm, Location, LocationNames}
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

  def printOverview(): Unit = {
    grid.positions sortWith { _.name < _.name} foreach { location: Location =>
      println(location.overview)
    }
  }
}

object World {
  def farmWorld: World = new World(startingGrid = Grid.allFarms)
  def cityWorld: World = new World(startingGrid = Grid.allCities)
  def fromGrid(grid: Grid): World = new World(startingGrid = grid)
  def fromLocations(locations: VectorGrid): World = new World(Grid(locations))
  def randomWorld: World = if (Random.nextInt(1) == 1) World.farmWorld else World.cityWorld
}

class Grid private (val positions: VectorGrid)
  extends Iterable[Location]
 {
  def fullPopulace: Populace = {
    positions.foldLeft(Populace.empty) { (a, loc) =>
      a ++ loc.populace
    }
  }

  override def iterator: Iterator[Location] = positions.iterator
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
      val farmName = LocationNames.nextName
      Farm(farmName, Populace.randomPop(15))
    }
  }

  def allCities: Grid = {
    fillGrid {
      val cityName = LocationNames.nextName
      City(cityName, Populace.randomPop(25))
    }
  }
}