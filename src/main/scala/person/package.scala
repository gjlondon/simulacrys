import org.joda.time.DateTime
import world.World

package object person {

  type ActionCandidates = List[(Commoner => Commoner, (DateTime, World, Commoner) => Boolean)]
}
