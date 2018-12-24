package message

import actions.Interaction
import entity.Entity

sealed trait Message {
  val from: Entity
  val to: Entity
  val payload: Interaction[Entity, Entity]
}
