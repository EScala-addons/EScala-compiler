package scala

package object events {

  def between[T,U](start: Event[T], end: Event[U]) = new BetweenEvent(start, end)

  def within[T,U](ie: IntervalEvent[T,U]) = new WithinEvent(ie)

  def causedBy[T](e: Event[T]) = new CausedByFilter(e)

  def ?[T](e: =>Event[T]) = new EventNodeCond(e)

  def allInstances[C]: VarList[C] = throw new NoSuchMethodException("this code has to be compiled with EScala")

  def anyInstance[C]: C = throw new NoSuchMethodException("this code has to be compiled with EScala")

}

// vim: set ts=2 sw=2 et:
