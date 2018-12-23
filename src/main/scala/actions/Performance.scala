package actions

import person.Commoner

sealed trait CurrentActivity

case object Idle extends CurrentActivity
case object Incapacitated extends CurrentActivity

sealed trait Performance[T <: Commoner] extends CurrentActivity {
  val of: Action[T]
  val ticksElapsed: Int
  def ticksRemaining: Int = of.ticksRequired - ticksElapsed
  def isComplete: Boolean = ticksRemaining == 0
}

case class CommonerPerformance(of: Action[Commoner],
                               ticksElapsed: Int = 1)
  extends Performance[Commoner] {
}