package scala.events

import scala.collection.mutable.ListBuffer

trait IntervalEvent[Start, Stop] {
  this: EventNode[_] =>

  val start: Event[Start]
  val end: Event[Stop]

  private var _active = false

  def active = _active

  private def onStart(v: Start) {
    if(!active) {
      started(v)
      _active = true
    }
  }

  private def onEnd(v: Stop) {
    if(active) {
      ended(v)
      _active = false
    }
  }

  /** Called when the interval event starts
  */
  protected[this] def started(v: Start)

  /** Called when the interval event stops
  */
  protected[this] def ended(v: Stop)

  protected def deploy {
    start += onStart _
    end += onEnd _
  }

  protected def undeploy {
    start -= onStart _
    end -= onEnd _
  }

}

class BetweenEventNode[T,U,V](val event: Event[T], val start: Event[U], val end: Event[V]) extends EventNode[T]
                                                                                           with IntervalEvent[U,V] {

  protected[this] def started(u: U) {
      event += onEvt _
  }

  protected[this] def ended(v: V) {
      event -= onEvt _
  }

  def onEvt(id: Int, value: T, reacts: ListBuffer[(() => Unit)]) {
    if(active)
      reactions(id, value, reacts)
  }

}

// vim: set ts=4 sw=4 et:
