package scala

package object events {

  def between[T](start: Event[T], end: Event[_]) = new BetweenEvent(start, end)

  private[events] def within[T, S, U](e: Event[T], ie: IntervalEvent[S], merge: (T, S) => U): Event[U] =
    (e && ie.active _ map ((t: T) => merge(t, ie.getValue))) || (e.and(ie.before, merge))

  private[events] def not_within[T](e: Event[T], ie: IntervalEvent[_]): Event[T] =
    (e && (_ => !ie.active)) \ ie.before
  private[events] def strictlyWithin[T](e: Event[T], ie: IntervalEvent[_]): Event[T] =
    (e && ie.active _) \ ie.after
  private[events] def not_strictlyWithin[T](e: Event[T], ie: IntervalEvent[_]): Event[T] =
    (e && (_ => !ie.active)) || e.and(ie.after, (t: T, s: Any) => t)

  def from[T](e: Event[T]) = new BetweenEvent[T](e, emptyevent)
  def to[T](e: Event[T]) = from(e).complement

  def causedBy[T](e: Event[T]) = new CausedByFilter(e)

  def ?[T](e: => Event[T]) = new EventNodeCond(e)
  
  def lazyWithin[T,S,U](e: Event[T], ie: => IntervalEvent[S], merge: (T,S) => U): Event[U] =
	  (e && (_ => ie.active) map ((t:T) => merge(t,ie.getValue))) || (new EventNodeAnd(e,ie.before,merge))
  def lazyAnd[T,S,U](e1: Event[T], e2: => Event[S], merge: (T,S) => U) : Event[U] =
	  new EventNodeAnd(e1,e2,merge)
  def lazyAnd[T,S](e1: Event[T], e2 : => Event[S]): Event[(T,S)] = lazyAnd(e1,e2,((t:T,s:S) => (t,s)))
  def lazyExcept[T](e1: Event[T], e2 : => Event[_]): Event[T] = new EventNodeExcept(e1,e2)
  def lazyNotWithin[T](e: Event[T], ie: => IntervalEvent[_]): Event[T] =
    lazyExcept((e && (_ => !ie.active)), ie.before)
  def lazyStrictlyWithin[T](e: Event[T], ie: => IntervalEvent[_]): Event[T] =
    lazyExcept((e && {_=> ie.active}), ie.after)
  def lazyNotStrictlyWithin[T](e: Event[T], ie: => IntervalEvent[_]): Event[T] =
    (e && (_ => !ie.active)) || lazyAnd[T,Any,T](e,ie.after, (t: T, s: Any) => t)
  
  
  
}

// vim: set ts=2 sw=2 et:
