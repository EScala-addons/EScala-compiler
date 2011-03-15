package scala

package object events {

  def causedBy[T](e: Event[T]) = new CausedByFilter(e)

  def ?[T](e: =>Event[T]) = new EventNodeCond(e)

}

// vim: set ts=2 sw=2 et:
