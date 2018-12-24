import facility.{Facility, FacilityGroup}

package object location {
  type Facilities = Map[FacilityGroup, List[Facility]]
}
