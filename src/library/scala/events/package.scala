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

  def from[T](e: Event[T]) = new BetweenEvent[T](e, emptyevent) {

    lazy val cleanUp: (Any => Unit) = ((_: Any) => {
      before.asInstanceOf[PunktualNode[T]].undeploy;
      after.asInstanceOf[PunktualNode[T]].undeploy;
      realStart -= cleanUp
    })

    override def deploy = {
      if (!active) {
        super.deploy
      }
      realStart += cleanUp
    }

  }
  def to[T](e: Event[T]) = from(e).complement

  def causedBy[T](e: Event[T]) = new CausedByFilter(e)

  def ?[T](e: => Event[T]) = new EventNodeCond(e)

}

// vim: set ts=2 sw=2 et:
