package scala

package object events {

  def between[T, U](start: Event[T], end: Event[U]) = new BetweenEvent(start, end)

  def within(e: Event[_], ie: IntervalEvent[_, _]) = (e && ie.active _) || (e and ie.before)
  def not_within(e: Event[_], ie: IntervalEvent[_,_]) = (e && (() => ! ie.active)) \ ie.before
  def strictlyWithin(e: Event[_], ie: IntervalEvent[_,_]) = (e && ie.active _) \ ie.complement.before
  def not_strictlyWithin(e: Event[_], ie: IntervalEvent[_,_]) = (e && (() => ! ie.active)) || ie.complement.before
  
  def causedBy[T](e: Event[T]) = new CausedByFilter(e)

  def ?[T](e: => Event[T]) = new EventNodeCond(e)

}

// vim: set ts=2 sw=2 et:
