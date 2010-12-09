package scala.events

import scala.collection.mutable.{ ListBuffer, Stack }

trait IntervalEvent[Start, Stop] {

  type Trace = List[Event[_]]

  def start: Event[Start]
  def end: Event[Stop]

  private lazy val realStart: Event[Start] = start && (_ => !active) && startCondition _
  private lazy val realEnd: Event[Stop] = end && (_ => active) && endCondition _

  protected[events] var deployed = false

  var defaultValue: Start = _
  var value: Start = _

  protected val startMergeBehaviour = (intervalValue: Start, changeEventValue: Start) => {
    //println("Merge-Interval-Value: " + intervalValue)
    //println("Event-Value: " + changeEventValue)
    if (intervalValue != this.defaultValue && changeEventValue != this.defaultValue)
      intervalValue;
    else
      changeEventValue;
  }

  protected[this] var _active = false
  def active = _active

  protected[this] def startCondition(v: Start) = true
  protected[this] def endCondition(v: Stop) = true

  protected[this] lazy val started = (s: Start) => {
    _active = true

    this.value = startMergeBehaviour(this.value, s)
    println("merged value is " + this.value)
  }

  protected[this] lazy val ended = (e: Stop) => {
    _active = false
    this.value = startMergeBehaviour(this.value, this.defaultValue)
    println("merged value is " + this.value)
  }

  val ref: ReferenceCounting = new ReferenceCounting {
    def deploy = IntervalEvent.this.deploy
    def undeploy = IntervalEvent.this.undeploy
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
  lazy val before: Event[Start] = new PunktualNode[Start](_before, ref)
  lazy val after: Event[Stop] = new PunktualNode[Stop](_after, ref)

  /**
   * the complementary interval (note that the start and end events are both
   * within an interval and it's complement)
   */
  lazy val complement = {
    val act = _active
    new BetweenEvent[Stop, Start](
      new PunktualNode[Stop](realEnd, ref),
      new PunktualNode[Start](realStart, ref)) {
      _active = !act
    }
  }

  /**
   * union of intervals, seen as sets of moments
   * (needs refinement when it comes to values)
   */
  def ||(ie: IntervalEvent[Start, Stop]) = {
    val act = _active
    val act2 = ie.active
    new BetweenEvent[Start, Stop](
      (realStart || ie.realStart),
      (((realEnd && (_ => !ie.active)) || (ie.realEnd && (_ => !active))
        || (realEnd.and(ie.realEnd, (s: Stop, v: Stop) => s))) \ (realStart || ie.realStart))) {

      _active = act || act2

    }
  }

  /**
   * intersection of intervals, seen as sets of moments
   * (need refinement for values)
   */
  def &&(ie: IntervalEvent[Start, Stop]) = new BetweenEvent[Start, Stop](
    ((realStart && (_ => ie.active)) || (ie.realStart && (_ => active)) || (realStart.and(ie.realStart, (s: Start, u: Start) => s))) \ (realEnd || ie.realEnd),
    realEnd || ie.realEnd) {
    _active = IntervalEvent.this._active && ie.active
  }

  /**
   * difference of intervals, seen as set of moments 
   * (note: this may need refining when it comes to values)
   */
  def \(ie: IntervalEvent[Stop, Start]) = this && ie.complement

}

protected[events] trait ReferenceCounting {
  /**
   * reference counting (for before/after wrappers)
   */
  private var refCount: Int = 0
  final def ++ {
    refCount += 1
    if (refCount == 1)
      deploy
  }
  final def -- {
    refCount -= 1
    if (refCount <= 0)
      undeploy

  }

  def deploy
  def undeploy
}
class PunktualNode[T](punktEv: Event[T], ref: ReferenceCounting) extends EventNode[T] {

  lazy val react = reactions _

  override def deploy {
    punktEv += react
    ref ++
  }

  override def undeploy {
    punktEv -= react
    ref --
  }
class BetweenEvent[T, U](val start: Event[T], val end: Event[U]) extends IntervalEvent[T, U]
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
protected[events] trait ReferenceCounting {
  /**
   * reference counting (for before/after wrappers)
   */
  private var refCount: Int = 0
  final def ++ {
    refCount += 1
    if (refCount == 1)
      deploy
  }
  final def -- {
    refCount -= 1
    if (refCount <= 0)
      undeploy

  }

  def deploy
  def undeploy
}