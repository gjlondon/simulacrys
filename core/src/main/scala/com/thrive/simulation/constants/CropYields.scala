package com.thrive.simulation.constants

import com.thrive.simulation.resource.{Beans, Meat, SimpleFood}

object CropYields {
  val means: Map[SimpleFood, Int] = Map[SimpleFood, Int](
    Beans -> 3,
    Meat -> 2
  )

  val stdevs: Map[SimpleFood, Int] = Map[SimpleFood, Int](
    Beans -> 4,
    Meat -> 1
  )
}
