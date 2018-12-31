package simulation.resource

sealed trait Quality

sealed trait Freshness extends Quality

case object Spoiled extends Freshness
case object Old extends Freshness
case object Fresh extends Freshness

sealed trait Durability extends Quality

case object Robust extends Durability
case object Fair extends Durability
case object Worn extends Durability
case object Broken extends Durability
