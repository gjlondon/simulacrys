package world

import location._
import populace.Populace

import scala.util.Random
import convenienceTypes.ConvenienceTypes.VectorGrid
import facility.Facility
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
  def randomWorld(startingTime: DateTime): World = {
    new World(Grid.randomGrid(startingTime))
  }
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

  def randomGrid(startingTime: DateTime,
                 size: Int = 100): Grid = {
    val positions = (0 to size).map { _ =>
      val locTypes = Array(Farm, Manor, City)

      val kls = Random.shuffle(locTypes.toList).head
      val randomPop = Populace.randomPop(15, startingTime)
      val facilities = Map[Facility, List[Facility]]()
      kls("here", randomPop, facilities)
    }.toVector

    Grid(positions)
  }

  def fillGrid[T <: Location](withElem: => T, size: Int = 10): Grid = {
    val positions: VectorGrid = Vector.fill[T](size * size)(withElem)
    Grid(positions)
  }

  def allFarms(startingTime: DateTime): Grid = {
    fillGrid {
      val farmName = LocationNames.nextName
      val facilities = Map[Facility, List[Facility]]()

      Farm(
        farmName,
        Populace.randomPop(15, startingTime),
        facilities
      )
    }
  }

  def allCities(startingTime: DateTime): Grid = {
    fillGrid {
      val cityName = LocationNames.nextName
      val facilities = Map[Facility, List[Facility]]()

      City(
        cityName,
        Populace.randomPop(25, startingTime),
        facilities
      )
    }
  }
}