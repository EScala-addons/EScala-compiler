package scala.events

import scala.collection.mutable.{ ListBuffer, Stack }

trait IntervalEvent[Start] {

  type Trace = List[Event[_]]

  def start: Event[Start]
  def end: Event[_]

  private lazy val realStart: Event[Start] = start && (_ => !active) && startCondition _
  private lazy val realEnd: Event[_] = end && (_ => active) && endCondition _

  protected[events] var deployed = false

  private final var default: Start = _
  protected def defaultValue = default
  protected var value: Start = _

  def getValue = value

  protected[this] var _active = false
  def active = _active

  protected[this] def startCondition(v: Start) = true
  protected[this] def endCondition(v: Any) = true

  protected[this] lazy val started = (s: Start) => {
    _active = true
    this.value = s
  }

  protected[this] lazy val ended = (e: Any) => {
    _active = false
    this.value = defaultValue
  }

  val ref = new ReferenceCounting {
    def deploy = IntervalEvent.this.deploy
    def undeploy = IntervalEvent.this.undeploy

    //DEBUG
    override def toString = IntervalEvent.this.toString
  }

  protected[events] def deploy {
    realStart += started
    realEnd += ended
    assert(deployed == false)
    deployed = true
  }

  protected[events] def undeploy {
    realStart -= started
    realEnd -= ended
    deployed = false
  }

  lazy val before: Event[Start] = new PunktualNode[Start](realStart, ref)
  lazy val after: Event[Start] = new PunktualNode[Start](realEnd.map((_: Any) => value), ref)

  /**
   * the complementary interval (note that the start and end events are both
   * within an interval and it's complement)
   */
  lazy val complement =
    new ActiveOverridingInterval[Unit](after dropParam, before, !active, ())

  /**
   * union of intervals, seen as sets of moments
   * (needs refinement when it comes to values)
   */
  def ||[S >: Start, S1 <: S](ie: IntervalEvent[S1]) = new ActiveOverridingInterval[S](
    (before || ie.before.asInstanceOf[Event[S]]),
    (((after && (_ => !ie.active)) || (ie.after && (_ => !active)) || (after and ie.after))) \ (before || ie.before),
    active || ie.active, value)

  /**
   * intersection of intervals, seen as sets of moments
   * (needs refinement for values)
   */
  def &&[S](ie: IntervalEvent[S]) = new ActiveOverridingInterval[(Start, S)](
    (((before && (_ => ie.active) map ((vals: Start) => (vals, ie.value)))
      || (ie.before && (_ => active) map ((vals: S) => (value, vals))))
      || (before.and(ie.before, (s: Start, u: S) => (s, u)))) \ (after || ie.after),
    after || ie.after,
    active && ie.active, (value, ie.value))
  /**
   * difference of intervals, seen as set of moments 
   */
  def \(ie: IntervalEvent[_]) = this && ie.complement map ((v: (Start, Unit)) => v._1)

  def map[S >: Start, T](f: S => T) =
    new ActiveOverridingInterval[T](before map f, after, active, f(value))

  def &&[S >: Start](p: S => Boolean) =
    new ActiveOverridingInterval(before && p, after, active && p(value), value)

  
  implicit def castValue[S >: Start] = map((v : Start) => v.asInstanceOf[S])
  /*
   * convenience methods
   */

  def and[T](e: Event[T]) = within(e, this)
  def and[T](ie: IntervalEvent[T]) = this && ie
  def and[S >: Start](p: S => Boolean) = this && p
  def or[T](ie: IntervalEvent[T]) = this || ie

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

protected[events] class PunktualNode[T](punktEv: Event[T], ref: ReferenceCounting) extends EventNode[T] {

  lazy val onEvt: Sink = (id: Int, v: T, reacts: ListBuffer[(() => Unit, List[Event[_]])]) => {
    reactions(id, v, reacts)
  }

  override def toString = punktEv.toString

  override def deploy {
    ref ++;
    punktEv += onEvt

  }

  override def undeploy {
    punktEv -= onEvt
    ref --
  }

  protected[events] override def redeploy {
    super.redeploy
    punktEv.redeploy
  }
}

class BetweenEvent[T](val start: Event[T], val end: Event[_]) extends IntervalEvent[T] {
  override def toString: String = "between(" + start + "," + end + ")"

}

/**
 * helper class for easy active and value initialization overriding 
 *
 */
protected[events] class ActiveOverridingInterval[T](start: Event[T],
  end: Event[_], defaultActive: Boolean, defValue: T) extends BetweenEvent(start, end) {
  _active = defaultActive
  value = if (active) defValue else defaultValue
}

class ExecutionEvent[T] extends IntervalEvent[T] {

  def start: Event[T] = _start
  def end: Event[_] = _end

  private var _start: Event[T] = _
  private var _end: Event[_] = _

  trait BeforeExecution {
    this: ImperativeEvent[T] =>
    _start = this
    protected[events] abstract override def afterTrigger(t: T) {
      cflow.push(t)
    }
  }

  trait AfterExecution[S] {
    this: ImperativeEvent[(T, S)] =>
    _end = this
    protected[events] abstract override def beforeTrigger(u: (T, S)) {
      cflow.pop
    }
  }

  private val cflow = Stack[T]()

  override def active = !cflow.isEmpty

  protected[this] override def endCondition(u: Any) = cflow.size == 1

  protected[events] override def deploy {}
  protected[events] override def undeploy {}

}