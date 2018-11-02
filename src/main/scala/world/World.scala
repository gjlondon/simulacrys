package world

import location.{City, Farm, Location}
import populace.Populace

import scala.util.Random

object LocationTypes {
  type VectorGrid = Vector[Vector[Location]]
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

  def fromGrid(grid: Grid): World = new World(startingGrid = grid)
}

class Grid private (startingPositions: VectorGrid) {
  def fullPopulace: Populace = {
    println("generating populace")
    val locations = for {
      col <- startingPositions
      loc <- col
    } yield loc
    println(locations)
    locations.foldLeft(Populace.empty) { (a, loc) =>
      a ++ loc.populace
    }
  }

  val positions: VectorGrid = startingPositions
}

object Grid {

  private def apply(startingPositions: VectorGrid): Grid = {
    new Grid(startingPositions = startingPositions)
  }

  def fillGrid[T <: Location](withElem: => T, size: Int = 10): Grid = {
    val contents: VectorGrid = Vector.fill[T](n1 = size, n2 = size)(withElem)
    Grid(contents)
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