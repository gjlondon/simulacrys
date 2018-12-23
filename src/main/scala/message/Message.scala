package message

import entity.Entity

sealed trait Message {
  val from: Entity
  val to: Entity
}
