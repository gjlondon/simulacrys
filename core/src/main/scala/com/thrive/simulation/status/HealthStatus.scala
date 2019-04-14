package com.thrive.simulation.status

import com.thrive.simulation.demographic._

sealed trait HealthStatus {
  val nextWorst: HealthStatus = {
    this match {
      case Robust => Fine
      case Fine => Poor
      case Poor => Sick
      case Sick => Dead
      case Dead => Dead
    }
  }

  val nextBest: HealthStatus = {
    this match {
      case Robust => Robust
      case Fine => Robust
      case Poor => Fine
      case Sick => Poor
      case Dead => Dead
    }
  }
}

object HealthStatus {
  def transitionProbabilities(ageBracket: AgeBracket): (Double, Double) = {
    val (worse, better) = ageBracket match {
      case Child => (.03, .10)
      case Young => (.01, .07)
      case Adult => (.03, .03)
      case Old => (.15, .01)
    }
    (worse, better)
  }
}

case object Robust extends HealthStatus
case object Fine extends HealthStatus
case object Poor extends HealthStatus
case object Sick extends HealthStatus
case object Dead extends HealthStatus
