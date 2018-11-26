package constants

import resource.{Beans, Meat, SimpleFood}

object CropYields {
  val means: Map[SimpleFood, Int] = Map[SimpleFood, Int](
    Beans -> 10,
    Meat -> 4
  )

  val stdevs: Map[SimpleFood, Int] = Map[SimpleFood, Int](
    Beans -> 4,
    Meat -> 2
  )
}
