package scala

package object events {

  def between[T,U](start: Event[T], end: Event[U]) = () // TODO

  def within[T,U](ie: IntervalEvent[T,U]) = ie.active _

}

// vim: set ts=2 sw=2 et:
