import simulation.actions.{Action, Reaction, Farm}
import simulation.location.Location
import simulation.message.Message
import org.joda.time.DateTime

package object facility {
  type ActionCandidates = List[
    (Action,
      (DateTime, Location, Action) => Boolean,
      Option[(Action, Location) => Option[Message]])]
}
