package resource

sealed trait Durability

case object Robust
case object Fair
case object Worn
case object Broken
