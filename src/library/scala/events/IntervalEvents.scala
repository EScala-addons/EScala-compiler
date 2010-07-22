package scala.events

import scala.collection.mutable.ListBuffer

class BetweenEventNode[T,U,V](val event: Event[T], val start: Event[U], val end: Event[V]) extends EventNode[T] {

  private[this] var active = false

  def onStart(u: U) {
    if(!active) {
      event += onEvt _
      active = true
    }
  }

  def onEnd(v: V) {
    if(active) {
      event -= onEvt _
      active = false
    }
  }

  def onEvt(id: Int, value: T, reacts: ListBuffer[(() => Unit)]) {
    if(active)
      reactions(id, value, reacts)
  }

  def deploy {
    start += onStart _
    end += onEnd _
  }

  def undeploy {
    start -= onStart _
    end -= onEnd _
  }

}

// vim: set ts=4 sw=4 et:
