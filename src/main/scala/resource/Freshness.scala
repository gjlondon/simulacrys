package resource

sealed trait Freshness

case object Spoiled extends Freshness
case object Old extends Freshness
case object Fresh extends Freshness