package scala

package object events {

  def between[T, U](start: Event[T], end: Event[U]) = new BetweenEvent(start, end)

  private[events] def within[T](e: Event[T], ie: IntervalEvent[_, _]): Event[T] = (e && ie.active _) || (e.and(ie.before, (t : T, s : Any )  => t))

  private[events] def not_within(e: Event[_], ie: IntervalEvent[_,_]) = (e && (() => ! ie.active)) \ ie.complement.after
  private[events] def strictlyWithin(e: Event[_], ie: IntervalEvent[_,_]) = (e && ie.active _) \ ie.complement.before
  private[events] def not_strictlyWithin(e: Event[_], ie: IntervalEvent[_,_]) = (e && (() => ! ie.active)) || ie.complement.before
  
  implicit def betweenFromTupled[T,U](t : (Event[T],Event[U])) = between(t._1,t._2)
  def not_within(e: Event[_], ie: IntervalEvent[_,_]) = (e && (() => ! ie.active)) \ ie.before
  def strictlyWithin(e: Event[_], ie: IntervalEvent[_,_]) = (e && ie.active _) \ ie.complement.before
  def not_strictlyWithin(e: Event[_], ie: IntervalEvent[_,_]) = (e && (() => ! ie.active)) || ie.complement.before
  
  def causedBy[T](e: Event[T]) = new CausedByFilter(e)

  def ?[T](e: => Event[T]) = new EventNodeCond(e)

}

// vim: set ts=2 sw=2 et:
