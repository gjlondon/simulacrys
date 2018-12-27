package actions

import java.util.UUID

import person.Commoner

sealed trait CurrentActivity

case object Idle extends CurrentActivity
case object Incapacitated extends CurrentActivity

sealed trait PerformanceStatus

case object PendingConfirmation extends PerformanceStatus
case object InProgress extends PerformanceStatus
case object Failed extends PerformanceStatus

sealed trait Performance[T <: Commoner] extends CurrentActivity {
  val perform: PersonAction
  val status: PerformanceStatus
  val ticksElapsed: Int
  val confirmsRequired: Mailbox
  def confirmed: Boolean = confirmsRequired.isEmpty
  def ticksRemaining: Int = perform.ticksRequired - ticksElapsed
  def isComplete: Boolean = ticksRemaining <= 0
  def advanceByTickIfConfirmed: Performance[T]
}

case class CommonerPerformance(perform: PersonAction,
                               ticksElapsed: Int = 0,
                               status: PerformanceStatus = PendingConfirmation,
                               confirmsRequired: Mailbox = Mailbox.empty)
  extends Performance[Commoner] {
  override def advanceByTickIfConfirmed: Performance[Commoner] = {
    if (confirmed) {
      this.copy(ticksElapsed = this.ticksElapsed + 1)
    }
    else this
  }
}