package com.thrive.simulation

import org.joda.time.DateTime
import com.thrive.simulation.actions.PersonAction
import com.thrive.simulation.location.Location
import com.thrive.simulation.message.Message

package object person {

  type ActionCandidates = List[
    (PersonAction,
    (DateTime, Location, Commoner) => Boolean,
    Option[(Commoner, Location) => Option[Message]])]
}
