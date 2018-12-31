package com.thrive.simulation.constants

import com.github.nscala_time.time.DurationBuilder
import squants.energy.SpecificEnergy
import com.thrive.simulation.resource.Calorie.calorie
import squants.mass.{AreaDensity, Kilograms}
import squants.space.Meters
import com.github.nscala_time.time.Imports._

object Constants {
  val ENERGY_PER_KILO_OF_FAT: SpecificEnergy = 7700 * calorie / Kilograms(1)
  val KG_PER_LB = 0.453592
  val LEAN_MASS_PER_METER_OF_HEIGHT: AreaDensity = Kilograms(75 * KG_PER_LB) / (Meters(1) * Meters(1))  // just guessing
  val TICK_DURATION: DurationBuilder = 5.minutes
}
