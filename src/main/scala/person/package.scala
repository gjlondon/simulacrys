import actions.{Action, Interaction, Reaction}
import entity.Entity
import org.joda.time.DateTime
import location.Location
import message.PersonToFacilityMessage

package object person {

  type ActionCandidates = List[
    (Action[Commoner],
    (DateTime, Location, Commoner) => Boolean,
    Option[(Commoner, Location) => Option[PersonToFacilityMessage]])]
  type ReactionCandidates = List[(Reaction, (DateTime, Location, Commoner) => Boolean)]
}
