import org.joda.time.DateTime
import world.World

package object person {

  type ActionCandidates = List[(String, Commoner => Commoner, (DateTime, World, Commoner) => Boolean)]
}
