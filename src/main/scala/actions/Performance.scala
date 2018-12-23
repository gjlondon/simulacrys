package actions

import person.Commoner

sealed trait CurrentActivity

case object Idle extends CurrentActivity
case object Incapacitated extends CurrentActivity

sealed trait Performance[T <: Commoner] extends CurrentActivity {
  val perform: Action[T]
  val ticksElapsed: Int
  def ticksRemaining: Int = perform.ticksRequired - ticksElapsed
  def isComplete: Boolean = ticksRemaining == 0

  def progress: Performance[T]
}

case class CommonerPerformance(perform: Action[Commoner],
                               ticksElapsed: Int = 0)
  extends Performance[Commoner] {
  override def progress: Performance[Commoner] = this.copy(ticksElapsed = this.ticksElapsed + 1)
}