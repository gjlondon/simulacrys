import actions.PersonAction
import location.Location
import message.Message
import org.joda.time.DateTime

package object person {

  type ActionCandidates = List[
    (PersonAction,
    (DateTime, Location, Commoner) => Boolean,
    Option[(Commoner, Location) => Option[Message]])]
}
