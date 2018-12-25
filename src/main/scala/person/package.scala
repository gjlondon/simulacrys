import actions.{Action, Reaction}
import location.Location
import message.Message
import org.joda.time.DateTime

package object person {

  type ActionCandidates = List[
    (Action[Commoner],
    (DateTime, Location, Commoner) => Boolean,
    Option[(Commoner, Location) => Option[Message]])]
  type ReactionCandidates = List[(Reaction, (DateTime, Location, Commoner) => Boolean)]
}
