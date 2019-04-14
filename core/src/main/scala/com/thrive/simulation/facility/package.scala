package com.thrive.simulation

import com.thrive.simulation.actions.{Action, Reaction, Farm}
import com.thrive.simulation.location.Location
import com.thrive.simulation.message.Message
import org.joda.time.DateTime

package object facility {
  type ActionCandidates = List[
    (Action,
      (DateTime, Location, Action) => Boolean,
      Option[(Action, Location) => Option[Message]])]
}
