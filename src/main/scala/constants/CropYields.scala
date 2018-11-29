package constants

import resource.{Beans, Meat, SimpleFood}

object CropYields {
  val means: Map[SimpleFood, Int] = Map[SimpleFood, Int](
    Beans -> 1,
    Meat -> 1
  )

  val stdevs: Map[SimpleFood, Int] = Map[SimpleFood, Int](
    Beans -> 4,
    Meat -> 2
  )
}
