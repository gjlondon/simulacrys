package com.thrive.simulation.facility

import com.thrive.simulation.message.Mailbox
import com.thrive.simulation.message.MailboxTypes.Mailbox

import scala.collection.generic.CanBuildFrom
import scala.collection.{SetLike, mutable}
import scala.language.postfixOps

class Facilities(seq : Facility*) extends Set[Facility]
  with SetLike[Facility, Facilities]
  with Serializable
{

  override def empty: Facilities = new Facilities()

  def + (elem: Facility) : Facilities = {
    if (seq contains elem) this
    else new Facilities(elem +: seq: _*)
  }

  def - (elem: Facility) : Facilities = {
    if (!(seq contains elem)) this
    else new Facilities(seq filterNot (elem ==): _*)
  }

  def contains (elem: Facility) : Boolean = seq exists (elem ==)

  def iterator : Iterator[Facility] = seq.iterator

  val outgoingMessages: Mailbox = {
    this.foldLeft(Mailbox.empty)((soFar, facility) => soFar ++ facility.outbox)
  }

  def get(key: FacilityGroup): Option[List[Facility]] = {
    val matches = seq.filter( _.grouping == key )
    if (matches.nonEmpty) Some(matches.toList) else None
  }
}

object Facilities {
  def empty: Facilities = new Facilities()
  def apply(elems: Facility*): Facilities = (empty /: elems) (_ + _)

  def newBuilder: mutable.Builder[Facility, Facilities] = new mutable.SetBuilder[Facility, Facilities](empty)

  implicit def canBuildFrom[A <: Facility]: CanBuildFrom[Facilities, A, Facilities] = new CanBuildFrom[Facilities, A, Facilities] {
    def apply(from: Facilities): mutable.Builder[A, Facilities] = newBuilder
    def apply(): mutable.Builder[A, Facilities] = newBuilder
  }
}