import actions.{Action, Reaction}
import org.joda.time.DateTime
import location.Location

package object person {

  type ActionCandidates = List[(Action[Commoner], (DateTime, Location, Commoner) => Boolean)]
  type ReactionCandidates = List[(Reaction, (DateTime, Location, Commoner) => Boolean)]
}
