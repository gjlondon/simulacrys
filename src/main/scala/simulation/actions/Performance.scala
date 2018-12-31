package simulation.actions

import java.util.UUID

import simulation.entity.Entity
import simulation.message._
import simulation.message.MailboxTypes.Outbox
import simulation.person.Commoner

import scala.collection.immutable.Queue

sealed trait CurrentActivity

case object Idle extends CurrentActivity
case object Incapacitated extends CurrentActivity

sealed trait PerformanceStatus

case object PendingConfirmation extends PerformanceStatus
case object InProgress extends PerformanceStatus
case object Failed extends PerformanceStatus

sealed trait Performance[T <: Commoner] extends CurrentActivity {
  val performer: T
  val perform: PersonAction
  val status: PerformanceStatus
  val ticksElapsed: Int
  val confirmsRequired: Map[UUID, Entity]
  val confirmationStatuses: Map[UUID, Boolean]

  def pendingConfirms: Set[UUID] = confirmationStatuses.collect {
    case (k, v: Boolean) if !v => k
  }.toSet
  def confirmed: Boolean = pendingConfirms.isEmpty
  def ticksRemaining: Int = perform.ticksRequired - ticksElapsed
  def isComplete: Boolean = ticksRemaining <= 0
  def advanceByTickIfConfirmed: Performance[T]
  def onFailure(): Outbox
  def onSuccess(): Outbox
}

case class CommonerPerformance private (perform: PersonAction,
                                        performer: Commoner,
                                        ticksElapsed: Int = 0,
                                        status: PerformanceStatus = PendingConfirmation,
                                        confirmsRequired: Map[UUID, Entity],
                                        confirmationStatuses: Map[UUID, Boolean])
  extends Performance[Commoner] {
  override def advanceByTickIfConfirmed: Performance[Commoner] = {
    if (confirmed) {
      this.copy(ticksElapsed = this.ticksElapsed + 1)
    }
    else this
  }

  override def onFailure(): Outbox = onSuccess()

  override def onSuccess(): Outbox = {
    val messages = confirmsRequired.values.map { e: Entity =>
      Request(
        from = performer.address,
        to = e.address,
        payload = Release
      )
    }.toSeq
    Queue[Message](messages: _*)
  }
}

object CommonerPerformance {
  def ofAction(action: PersonAction,
               by: Commoner,
               confirmsRequired: Map[UUID, Entity],
              ): CommonerPerformance = {


    val confirmationStatuses: Map[UUID, Boolean] =
      confirmsRequired.keys.map(_ -> false).toMap

    CommonerPerformance(perform = action,
      performer = by,
    status = PendingConfirmation,
    confirmsRequired = confirmsRequired,
    confirmationStatuses = confirmationStatuses)
  }
}