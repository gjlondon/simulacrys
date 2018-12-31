import simulation.actions.PersonAction
import simulation.location.Location
import simulation.message.Message
import org.joda.time.DateTime
import simulation.person.Commoner

package object simulation {

  type ActionCandidates = List[
    (PersonAction,
    (DateTime, Location, Commoner) => Boolean,
    Option[(Commoner, Location) => Option[Message]])]
}
