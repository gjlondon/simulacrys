package world

import location.{City, Farm, Location, LocationNames}
import populace.Populace

import scala.util.Random
import convenienceTypes.ConvenienceTypes.VectorGrid
import org.joda.time.DateTime

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

  def printSummary(): Unit = {
    val totalPop: Int = totalPopulation
    println(s"Total world population $totalPop")
  }

  def totalPopulation: Int = {
    val allPops = grid.positions map { loc: Location =>
      loc.livingPopSize
    }
    allPops.sum
  }
}

object World {
  def farmWorld(startingTime: DateTime): World = new World(
    startingGrid = Grid.allFarms(startingTime)
  )
  def cityWorld(startingTime: DateTime): World = new World(
    startingGrid = Grid.allCities(startingTime)
  )

  def fromGrid(grid: Grid): World = new World(startingGrid = grid)
  def fromLocations(locations: VectorGrid): World = new World(Grid(locations))
  def randomWorld(startingTime: DateTime): World = if (Random.nextInt(1) == 1)
    World.farmWorld(startingTime) else World.cityWorld(startingTime)
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

  def allFarms(startingTime: DateTime): Grid = {
    fillGrid {
      val farmName = LocationNames.nextName
      Farm(farmName, Populace.randomPop(15, startingTime))
    }
  }

  def allCities(startingTime: DateTime): Grid = {
    fillGrid {
      val cityName = LocationNames.nextName
      City(cityName, Populace.randomPop(25, startingTime))
    }
  }
}