import actions.Action
import org.joda.time.DateTime
import world.World

package object person {

  type ActionCandidates = List[(Action[Commoner], (DateTime, World, Commoner) => Boolean)]
}
