package scala

package object events {

  def between[T,U](start: Event[T], end: Event[U]) = new BetweenEvent(start, end)

  def within[T,U](ie: IntervalEvent[T,U]) = ie.active _

}

// vim: set ts=2 sw=2 et:
