package status

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
}

case object Robust extends HealthStatus
case object Fine extends HealthStatus
case object Poor extends HealthStatus
case object Sick extends HealthStatus
case object Dead extends HealthStatus
