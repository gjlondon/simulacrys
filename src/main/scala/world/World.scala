package world

import location.{City, Farm, Location}

import scala.util.Random

object LocationTypes {
  type VectorGrid = Vector[Vector[Location]]
}

import LocationTypes.VectorGrid

class World {
  val grid: Grid = Grid.allFarms
}

class Grid private (startingPositions: VectorGrid) {
  val positions: VectorGrid = startingPositions
}

object Grid {

  private def apply(startingPositions: VectorGrid): Grid = {
    new Grid(startingPositions = startingPositions)
  }

  def fillGrid[T <: Location](withElem: => T, size: Int = 100): Grid = {
    val contents: VectorGrid = Vector.fill[T](n1 = size, n2 = size)(withElem)
    Grid(contents)
  }

  def allFarms: Grid = {
    fillGrid {
      val farmName = Random.nextString(5)
      Farm(farmName)
    }
  }

  def allCities: Grid = {
    fillGrid {
      val cityName = Random.nextString(5)
      City(cityName)
    }
  }
}