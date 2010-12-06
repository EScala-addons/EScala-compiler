package scala.events

import scala.collection.mutable.{ ListBuffer, Stack }

trait IntervalEvent[+Start, +Stop] {

  type Trace = List[Event[_]]

  def start: Event[Start]
  def end: Event[Stop]

  private lazy val realStart: Event[Start] = start && (_ => !active) && startCondition _
  private lazy val realEnd: Event[Stop] = end && (_ => active) && endCondition _

  protected[events] var deployed = false

  protected[this] var _active = false
  def active = _active

  protected[this] def startCondition(v: Start) = true
  protected[this] def endCondition(v: Stop) = true

  protected[this] lazy val started = (s: Start) => {
    _active = true
  }

  protected[this] lazy val ended = (e: Stop) => {
    _active = false
  }

  protected[this] var refCount: Int = 0
  protected[events] def incref {
    refCount += 1
    if (refCount == 1)
      deploy
  }
  protected[events] def decref {
    refCount -= 1
    if (refCount <= 0)
      undeploy

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

  protected[events] def _before = realStart
  protected[events] def _after = realEnd
  lazy val before: Event[Start] = new PunktualNode[Start](_before, incref _, decref _)
  lazy val after: Event[Stop] = new PunktualNode[Stop](_after, incref _, decref _)

  lazy val complement = {
	  val act = _active
	  new BetweenEvent[Stop, Start](realEnd, realStart) {
    _active = ! act
  }}

  def ||[T >: Start, U >: Stop](ie: IntervalEvent[T, U]) = {
	  val act = _active
	  val act2 = ie.active
	  new BetweenEvent[T, U](
	(realStart || ie.realStart) ,
    (((realEnd && (_ => !ie.active)) || (ie.realEnd && (_ => !active))
      || (realEnd.and(ie.realEnd, (s: Stop, v: U) => s))) \ (realStart || ie.realStart))
      ){
  
	 	  _active = act || act2
	   
	   }}

  def &&[T >: Start, U >: Stop](ie: IntervalEvent[T,U]) = new BetweenEvent[T,U](
		  ((realStart && (_ => ie.active)) || (ie.realStart && (_=>active)) ||
		  (realStart.and(ie.realStart,(s:Start,u:T) => s))) \ (realEnd || ie.realEnd),
		  realEnd || ie.realEnd
  ){
	  _active = IntervalEvent.this._active && ie.active
  }
      
}

class PunktualNode[T](punktEv: Event[T], incref: (() => Unit), decref: (() => Unit)) extends EventNode[T] {

  lazy val react = reactions _

  override def deploy {
    punktEv += react
    incref()
  }

  override def undeploy {
    punktEv -= react
    decref()
  }
}

class BetweenEvent[+T, +U](val start: Event[T], val end: Event[U]) extends IntervalEvent[T, U]

class ExecutionEvent[T, U] extends IntervalEvent[T, U] {

  def start: Event[T] = _start
  def end: Event[U] = _end

  private var _start: Event[T] = _
  private var _end: Event[U] = _

  trait BeforeExecution {
    this: ImperativeEvent[T] =>
    _start = this
    protected[events] abstract override def afterTrigger(t: T) {
      cflow.push(t)
    }
  }

  trait AfterExecution {
    this: ImperativeEvent[U] =>
    _end = this
    protected[events] abstract override def beforeTrigger(u: U) {
      cflow.pop
    }
  }

  private val cflow = Stack[T]()

  override def active = !cflow.isEmpty

  protected[this] override def endCondition(u: U) = cflow.size == 1

  protected[events] override def deploy {}
  protected[events] override def undeploy {}

  override protected[events] def _before = start
  override protected[events] def _after = end

}
