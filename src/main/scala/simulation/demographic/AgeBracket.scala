package simulation.demographic

sealed trait AgeBracket

case object Child extends AgeBracket
case object Young extends AgeBracket
case object Adult extends AgeBracket
case object Old extends AgeBracket

