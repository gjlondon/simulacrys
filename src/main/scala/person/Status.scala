package person

sealed trait Status {

}

sealed trait ActivityStatus extends Status

case object Idle extends ActivityStatus
case object Busy extends ActivityStatus
case object Incapacitated extends ActivityStatus