import actions.{Action, Reaction}
import location.Location
import message.Message
import org.joda.time.DateTime

package object facility {
  type ActionCandidates = List[
    (Action[Farm],
      (DateTime, Location, Farm) => Boolean,
      Option[(Farm, Location) => Option[Message]])]
  type ReactionCandidates = List[(Reaction[Farm], (DateTime, Location, Farm) => Boolean)]
}
