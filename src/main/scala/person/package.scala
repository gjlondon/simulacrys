import actions.{Action, Reaction}
import org.joda.time.DateTime
import world.World

package object person {

  type ActionCandidates = List[(Action[Commoner], (DateTime, World, Commoner) => Boolean)]
  type ReactionCandidates = List[(Reaction, (DateTime, World, Commoner) => Boolean)]
}
