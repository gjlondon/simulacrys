package simulation.location

import simulation.entity.Entity
import simulation.facility._
import org.joda.time.DateTime
import simulation.person.Person
import simulation.populace.Populace

import scala.util.Random


sealed trait Location {
  def overview: String = {
    s"Location $name of type ${this.getClass.getSimpleName} has a living simulation.populace of size ${this.livingPopSize}"
  }

  val facilities: Facilities

  def hasAvailableFacility(facilityGroup: FacilityGroup): Boolean = {
    facilities.get(facilityGroup) match {
      case None => false
      case Some(matchingFacilities) => matchingFacilities.find(f => f.isAvailable ) match {
        case None => false
        case Some(_) => true
      }
    }
  }

  def findAvailableFacility(facilityGroup: FacilityGroup): Option[Facility] = {
    facilities.get(facilityGroup) match {
      case None => None
      case Some(matchingFacilities) => matchingFacilities.find(f => f.isAvailable ) match {
        case None => None
        case Some(facility) => Some(facility)
      }
    }
  }

  val name: String
  val populace: Populace

  val entities: Set[Entity] = populace ++ facilities
  def livingPopulace: Populace = populace.living
  def livingPopSize: Int = livingPopulace.size
  def withNewEntities(entities: Set[Entity]): Location  // TODO maybe replace with a lens?
}

case class City(name: String,
                populace: Populace,
                facilities: Facilities) extends Location {
  override def withNewEntities(entities: Set[Entity]): Location = {
    // TODO can this be a groupby to avoid two passes?
    val people = entities.collect {case p: Person => p }.toSeq
    val populace = Populace(people: _*)

    val facilities = Facilities(
      entities.collect {case f: Facility => f }.toSeq: _*
    )

    this.copy(populace = populace.living, facilities = facilities)
  }
}

object City {
  def buildRandom(startingTime: DateTime): City = {
    val name = LocationNames.nextName
    val randomPop = Populace.randomPop(ofSize = typicalPopulation, startingTime)
    val facilities = typicalFacilities
    City(name, randomPop, facilities)
  }

  def typicalFacilities: Facilities = {
    Facilities(
      simulation.facility.Farm(),
      simulation.facility.Pasture(),
      simulation.facility.Forest()
    )
  }

  val typicalPopulation: Int = 25
}

case class Manor(name: String,
                 populace: Populace,
                 facilities: Facilities) extends Location {
  override def withNewEntities(entities: Set[Entity]): Location = {
    // TODO can this be a groupby to avoid two passes?
    val people = entities.collect {case p: Person => p }.toSeq
    val populace = Populace(people: _*)

    val facilities = Facilities(
      entities.collect {case f: Facility => f }.toSeq: _*
    )

    this.copy(populace = populace.living, facilities = facilities)
  }
}

object Manor {
  def buildRandom(startingTime: DateTime): Manor = {
    val name = LocationNames.nextName
    val randomPop = Populace.randomPop(ofSize = typicalPopulation, startingTime)
    val facilities = typicalFacilities
    Manor(name, randomPop, facilities)
  }

  val typicalPopulation: Int = 15

  def typicalFacilities: Facilities = {
    Facilities(
      simulation.facility.Farm(),
      simulation.facility.Farm(),
      simulation.facility.Pasture(),
      simulation.facility.Pasture(),
      simulation.facility.Forest(),
      simulation.facility.Forest()
    )
  }
}

case class Farm(name: String,
                populace: Populace,
                facilities: Facilities) extends Location {
  override def withNewEntities(entities: Set[Entity]): Location = {
    // TODO can this be a groupby to avoid two passes?
    val people = entities.collect {case p: Person => p }.toSeq
    val populace = Populace(people: _*)

    val facilities = Facilities(
      entities.collect {case f: Facility => f }.toSeq: _*
    )

    this.copy(populace = populace.living, facilities = facilities)
  }

}

object Farm {
  def buildRandom(startingTime: DateTime): Farm = {
    val name = LocationNames.nextName
    val randomPop = Populace.randomPop(ofSize = typicalPopulation, startingTime)
    val facilities = typicalFacilities
    Farm(name, randomPop, facilities)
  }

  val typicalPopulation: Int = 10

  def typicalFacilities: Facilities = {
    Facilities(
      simulation.facility.Farm(),
      simulation.facility.Farm(),
      simulation.facility.Pasture(),
      simulation.facility.Pasture(),
      simulation.facility.Forest(),
      simulation.facility.Forest()
    )
  }
}



object LocationNames {

  def nextName: String = names(Random.nextInt(names.length))

  val names = List(
    "Pinnella Pass",
    "Fernsworth",
    "Wimborne",
    "Culcheth",
    "Gillamoor",
    "Azmarin",
    "Middlesborough",
    "Falkirk",
    "Haling Cove",
    "Baerney",
    "Three Streams",
    "Kara's Vale",
    "Norbury",
    "Leefside",
    "Tardide",
    "Tardide",
    "Ardglass",
    "Braedon",
    "Edinborourgh",
    "Bury",
    "Aylesbury",
    "Streatham",
    "Emall",
    "Boatwright",
    "Langdale",
    "Emall",
    "Anghor Wat",
    "Boroughton",
    "Stratford",
    "Rivermouth",
    "Irragin",
    "Boatwright",
    "Quan Ma",
    "Dawsbury",
    "Hull",
    "Lundy",
    "Lybster",
    "Clare View Point",
    "Lancaster",
    "Warlington",
    "Strathmore",
    "Hollyhead",
    "Pirn",
    "Azmar",
    "Davenport",
    "Redwick Bush",
    "Beggar's Hole",
    "Kilkenny",
    "Chaepstow",
    "Calmarnock",
    "Strathmore",
    "Hollyhead",
    "Pirn",
    "Azmar",
    "Davenport",
    "Redwick Bush",
    "Beggar's Hole",
    "Kilkenny",
    "Chaepstow",
    "Calmarnock",
    "Bamburgh",
    "Stathford",
    "Lunaris",
    "Aysgarth",
    "Auchterarder",
    "Baerney",
    "Hollyhead",
    "Gramsby",
    "Rutherglen",
    "Peterborough",
    "Bexley",
    "Sharpton",
    "Kinallen",
    "Hillfar",
    "Everwinter",
    "Taewe",
    "Carningsby",
    "Achnasheen",
    "Haran",
    "Solaris",
    
    // east
    "East Pinnella Pass",
    "East Fernsworth",
    "East Wimborne",
    "East Culcheth",
    "East Gillamoor",
    "East Azmarin",
    "East Middlesborough",
    "East Falkirk",
    "East Haling Cove",
    "East Baerney",
    "East Three Streams",
    "East Kara's Vale",
    "East Norbury",
    "East Leefside",
    "East Tardide",
    "East Tardide",
    "East Ardglass",
    "East Braedon",
    "East Edinborourgh",
    "East Bury",
    "East Aylesbury",
    "East Streatham",
    "East Emall",
    "East Boatwright",
    "East Langdale",
    "East Emall",
    "East Anghor Wat",
    "East Boroughton",
    "East Stratford",
    "East Rivermouth",
    "East Irragin",
    "East Boatwright",
    "East Quan Ma",
    "East Dawsbury",
    "East Hull",
    "East Lundy",
    "East Lybster",
    "East Clare View Point",
    "East Lancaster",
    "East Warlington",
    "East Strathmore",
    "East Hollyhead",
    "East Pirn",
    "East Azmar",
    "East Davenport",
    "East Redwick Bush",
    "East Beggar's Hole",
    "East Kilkenny",
    "East Chaepstow",
    "East Calmarnock",
    "East Strathmore",
    "East Hollyhead",
    "East Pirn",
    "East Azmar",
    "East Davenport",
    "East Redwick Bush",
    "East Beggar's Hole",
    "East Kilkenny",
    "East Chaepstow",
    "East Calmarnock",
    "East Bamburgh",
    "East Stathford",
    "East Lunaris",
    "East Aysgarth",
    "East Auchterarder",
    "East Baerney",
    "East Hollyhead",
    "East Gramsby",
    "East Rutherglen",
    "East Peterborough",
    "East Bexley",
    "East Sharpton",
    "East Kinallen",
    "East Hillfar",
    "East Everwinter",
    "East Taewe",
    "East Carningsby",
    "East Achnasheen",
    "East Haran",
    "East Solaris",
    
    // South
    "South Pinnella Pass",
    "South Fernsworth",
    "South Wimborne",
    "South Culcheth",
    "South Gillamoor",
    "South Azmarin",
    "South Middlesborough",
    "South Falkirk",
    "South Haling Cove",
    "South Baerney",
    "South Three Streams",
    "South Kara's Vale",
    "South Norbury",
    "South Leefside",
    "South Tardide",
    "South Tardide",
    "South Ardglass",
    "South Braedon",
    "South Edinborourgh",
    "South Bury",
    "South Aylesbury",
    "South Streatham",
    "South Emall",
    "South Boatwright",
    "South Langdale",
    "South Emall",
    "South Anghor Wat",
    "South Boroughton",
    "South Stratford",
    "South Rivermouth",
    "South Irragin",
    "South Boatwright",
    "South Quan Ma",
    "South Dawsbury",
    "South Hull",
    "South Lundy",
    "South Lybster",
    "South Clare View Point",
    "South Lancaster",
    "South Warlington",
    "South Strathmore",
    "South Hollyhead",
    "South Pirn",
    "South Azmar",
    "South Davenport",
    "South Redwick Bush",
    "South Beggar's Hole",
    "South Kilkenny",
    "South Chaepstow",
    "South Calmarnock",
    "South Strathmore",
    "South Hollyhead",
    "South Pirn",
    "South Azmar",
    "South Davenport",
    "South Redwick Bush",
    "South Beggar's Hole",
    "South Kilkenny",
    "South Chaepstow",
    "South Calmarnock",
    "South Bamburgh",
    "South Stathford",
    "South Lunaris",
    "South Aysgarth",
    "South Auchterarder",
    "South Baerney",
    "South Hollyhead",
    "South Gramsby",
    "South Rutherglen",
    "South Peterborough",
    "South Bexley",
    "South Sharpton",
    "South Kinallen",
    "South Hillfar",
    "South Everwinter",
    "South Taewe",
    "South Carningsby",
    "South Achnasheen",
    "South Haran",
    "South Solaris",

    // North
    "North Pinnella Pass",
    "North Fernsworth",
    "North Wimborne",
    "North Culcheth",
    "North Gillamoor",
    "North Azmarin",
    "North Middlesborough",
    "North Falkirk",
    "North Haling Cove",
    "North Baerney",
    "North Three Streams",
    "North Kara's Vale",
    "North Norbury",
    "North Leefside",
    "North Tardide",
    "North Tardide",
    "North Ardglass",
    "North Braedon",
    "North Edinborourgh",
    "North Bury",
    "North Aylesbury",
    "North Streatham",
    "North Emall",
    "North Boatwright",
    "North Langdale",
    "North Emall",
    "North Anghor Wat",
    "North Boroughton",
    "North Stratford",
    "North Rivermouth",
    "North Irragin",
    "North Boatwright",
    "North Quan Ma",
    "North Dawsbury",
    "North Hull",
    "North Lundy",
    "North Lybster",
    "North Clare View Point",
    "North Lancaster",
    "North Warlington",
    "North Strathmore",
    "North Hollyhead",
    "North Pirn",
    "North Azmar",
    "North Davenport",
    "North Redwick Bush",
    "North Beggar's Hole",
    "North Kilkenny",
    "North Chaepstow",
    "North Calmarnock",
    "North Strathmore",
    "North Hollyhead",
    "North Pirn",
    "North Azmar",
    "North Davenport",
    "North Redwick Bush",
    "North Beggar's Hole",
    "North Kilkenny",
    "North Chaepstow",
    "North Calmarnock",
    "North Bamburgh",
    "North Stathford",
    "North Lunaris",
    "North Aysgarth",
    "North Auchterarder",
    "North Baerney",
    "North Hollyhead",
    "North Gramsby",
    "North Rutherglen",
    "North Peterborough",
    "North Bexley",
    "North Sharpton",
    "North Kinallen",
    "North Hillfar",
    "North Everwinter",
    "North Taewe",
    "North Carningsby",
    "North Achnasheen",
    "North Haran",
    "North Solaris",
  
    // West
    "West Pinnella Pass",
    "West Fernsworth",
    "West Wimborne",
    "West Culcheth",
    "West Gillamoor",
    "West Azmarin",
    "West Middlesborough",
    "West Falkirk",
    "West Haling Cove",
    "West Baerney",
    "West Three Streams",
    "West Kara's Vale",
    "West Norbury",
    "West Leefside",
    "West Tardide",
    "West Tardide",
    "West Ardglass",
    "West Braedon",
    "West Edinborourgh",
    "West Bury",
    "West Aylesbury",
    "West Streatham",
    "West Emall",
    "West Boatwright",
    "West Langdale",
    "West Emall",
    "West Anghor Wat",
    "West Boroughton",
    "West Stratford",
    "West Rivermouth",
    "West Irragin",
    "West Boatwright",
    "West Quan Ma",
    "West Dawsbury",
    "West Hull",
    "West Lundy",
    "West Lybster",
    "West Clare View Point",
    "West Lancaster",
    "West Warlington",
    "West Strathmore",
    "West Hollyhead",
    "West Pirn",
    "West Azmar",
    "West Davenport",
    "West Redwick Bush",
    "West Beggar's Hole",
    "West Kilkenny",
    "West Chaepstow",
    "West Calmarnock",
    "West Strathmore",
    "West Hollyhead",
    "West Pirn",
    "West Azmar",
    "West Davenport",
    "West Redwick Bush",
    "West Beggar's Hole",
    "West Kilkenny",
    "West Chaepstow",
    "West Calmarnock",
    "West Bamburgh",
    "West Stathford",
    "West Lunaris",
    "West Aysgarth",
    "West Auchterarder",
    "West Baerney",
    "West Hollyhead",
    "West Gramsby",
    "West Rutherglen",
    "West Peterborough",
    "West Bexley",
    "West Sharpton",
    "West Kinallen",
    "West Hillfar",
    "West Everwinter",
    "West Taewe",
    "West Carningsby",
    "West Achnasheen",
    "West Haran",
    "West Solaris",
  )
}