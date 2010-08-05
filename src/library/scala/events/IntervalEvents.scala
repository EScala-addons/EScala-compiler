package scala.events

import scala.collection.mutable.{ListBuffer,Stack}

trait IntervalEvent[+Start, +Stop] {

  def start: Event[Start]
  def end: Event[Stop]

  private lazy val realStart: Event[Start] = start && (_ => !active) && startCondition _
  private lazy val realEnd: Event[Stop] = end && (_ => active) && endCondition _

  protected[events] var deployed = false

  protected[this] var _active = false
  def active = _active

  protected[this] def startCondition(v: Start) = true
  protected[this] def endCondition(v: Stop) = true

  protected[this] def onStart(v: Start): Unit

  protected[this] def onEnd(v: Stop): Unit

  protected[this] lazy val started = (v: Start) => {
    onStart(v)
    _active = true
  }

  protected[this] lazy val ended = (v: Stop) => {
    onEnd(v)
    _active = false
  }

  protected[events] def deploy {
    realStart += started
    realEnd += ended
    deployed = true
  }

  protected[events] def undeploy {
    realStart -= started
    realEnd -= ended
    deployed = false
  }

}

class BetweenEvent[T,U](val start: Event[T], val end: Event[U]) extends IntervalEvent[T,U] {

  protected[this] def onStart(t: T) {
  }

  protected[this] def onEnd(u: U) {
  }

}

class ExecutionEvent[T,U](val start: ImperativeEvent[T], val end: ImperativeEvent[U]) extends IntervalEvent[T,U] {

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
