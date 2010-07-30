package scala.events

import scala.collection.mutable.{ListBuffer,Stack}

trait IntervalEvent[Start, Stop] {
  this: EventNode[_] =>

  def start: Event[Start]
  def end: Event[Stop]

  private evt realStart[Start] = start && (_ => !active) && startCondition _
  private evt realEnd[Stop] = end && (_ => active) && endCondition _

  protected[this] var _active = false

  def active = _active

  protected[this] def startCondition(v: Start) = true
  protected[this] def endCondition(v: Stop) = true

  protected[this] def onStart(v: Start): Unit

  protected[this] def onEnd(v: Stop): Unit

  private def started(v: Start) {
    onStart(v)
    _active = true
  }

  private def ended(v: Stop) {
    onEnd(v)
    _active = false
  }

  protected def deploy {
    realStart += started _
    realEnd += ended _
  }

  protected def undeploy {
    realStart -= started _
    realEnd -= ended _
  }

}

class BetweenEventNode[T,U,V](val event: Event[T], val start: Event[U], val end: Event[V]) extends EventNode[T]
                                                                                           with IntervalEvent[U,V] {

  protected[this] def onStart(u: U) {
      event += onEvt _
  }

  protected[this] def onEnd(v: V) {
      event -= onEvt _
  }

  def onEvt(id: Int, value: T, reacts: ListBuffer[(() => Unit)]) {
      reactions(id, value, reacts)
  }

}

class Execution[T,U](val start: ImperativeEvent[T], val end: ImperativeEvent[U]) extends EventNode[Nothing]
                                                                                 with    IntervalEvent[T,U] {

  private val cflow = Stack[T]()

  override def active = !cflow.isEmpty

  protected[this] override def endCondition(u: U) = cflow.size == 1

  protected[this] def onStart(t: T) {
    cflow.push(t)
  }

  protected[this] def onEnd(u: U) {
    cflow.pop
  }

}

// vim: set ts=4 sw=4 et:
