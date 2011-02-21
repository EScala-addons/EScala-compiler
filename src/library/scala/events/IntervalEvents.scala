package scala.events

import scala.collection.mutable.{ ListBuffer, Stack }

trait IntervalEvent[Start] {

  type Sink = (Int, Start, ListBuffer[(() => Unit, Trace)]) => Unit
  type Trace = List[Event[_]]

  def start: Event[Start]
  def end: Event[_]

  protected lazy val realStart: Event[Start] = new BeforeNode[Start](start && (_ => !active) && startCondition _ )
  private lazy val realEnd: Event[_] = new BeforeNode[Any](end && (_ => active) && endCondition _ )

  private final var default: Start = _
  protected def defaultValue = default
  protected var _value: Start = _

  /**
   * this is valid if and only if active is valid also, use value for reliable pull-based
   * values if you really need to
   * @return
   */
  protected[events] def getValue = _value
  /**
   * use this to access this Interval's value. Note that subclasses of IntervalEvent should
   * use getValue instead and treat the special case of before explicitly to avoid the
   * performance overhead of pull
   * @return
   */
  def value : Start = {if (eventTrace.value.isEmpty) getValue //No events triggered
		  			else if (isActive) { //pull activation
		  				if (active) getValue //realStart already fired or we were active before
		  				else realStart.pullIsActivated(EventIds.lastId).getOrElse(defaultValue)
		  				}
		  			else defaultValue }

  protected[this] var _active = false
  protected[events] def active = _active
  /**
   * use this to access this Interval's activation state. Note that subclasses should use
   * active insted and treat the special case of before explicitly to avoid the performance
   * overhead of pull
   * @return
   */
  def isActive = {if(eventTrace.value.isEmpty) active 
		  			else if(active) realEnd.pullIsActivated(EventIds.lastId) == None
		  			else realStart.pullIsActivated(EventIds.lastId) != None
  }

  protected[this] def startCondition(v: Start) = true
  protected[this] def endCondition(v: Any) = true

  protected[this] lazy val started = (s: Start) => {
    _active = true
    this._value = s
  }

  protected[this] lazy val ended = (s:Any) => {
    _active = false
    this._value = defaultValue
  }

  val ref = new ReferenceCounting {
    def deploy = IntervalEvent.this.deploy
    def undeploy = IntervalEvent.this.undeploy

  }

  protected[events] def deploy {
    realStart += started
    realEnd += ended
  }

  protected[events] def undeploy {
    realStart -= started
    realEnd -= ended
  }

  lazy val before: Event[Start] = new PunktualNode[Start](realStart, ref)
  lazy val after: Event[Start] = new PunktualNode[Start](realEnd.map((_: Any) => _value), ref)

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
    active || ie.active, _value)

  /**
   * intersection of intervals, seen as sets of moments
   * (needs refinement for values)
   */
  def &&[S](ie: IntervalEvent[S]) = new ActiveOverridingInterval[(Start, S)](
    (((before && (_ => ie.active) map ((vals: Start) => (vals, ie._value)))
      || (ie.before && (_ => active) map ((vals: S) => (_value, vals))))
      || (before.and(ie.before, (s: Start, u: S) => (s, u)))) \ (after || ie.after),
    after || ie.after,
    active && ie.active, (_value, ie._value))
  /**
   * difference of intervals, seen as set of moments 
   */
  def \(ie: IntervalEvent[_]) = this && ie.complement map ((v: (Start, Unit)) => v._1)

  def map[S >: Start, T](f: S => T) =
    new ActiveOverridingInterval[T](before map f, after, active, f(_value))

  def &&[S >: Start](p: S => Boolean) =
    new ActiveOverridingInterval(before && p, after, active && p(_value), _value)

  
  implicit def castValue[S >: Start] = map((v : Start) => v.asInstanceOf[S])
  /*
   * convenience methods
   */

  def and[T](e: Event[T]) = within(e, this, (t:T,s:Start) => t)
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

 // override def toString = punktEv.toString

  override def deploy {
    ref ++;	// deploy Interval first, to avoid problems with indirect event triggering
    punktEv += onEvt

  }

  override protected[events] def undeploy {
    punktEv -= onEvt
    ref --
  }
  
  protected override def pullParents(Id : Int): Option[T] = punktEv.pullIsActivated(Id)
}

class BetweenEvent[T](val start: Event[T], val end: Event[_]) extends IntervalEvent[T] {
//  override def toString: String = "between(" + start + "," + end + ")"

}

/**
 * helper class for easy active and value initialization overriding 
 *
 */
protected[events] class ActiveOverridingInterval[T](start: Event[T],
  end: Event[_], defaultActive: Boolean, defValue: T) extends BetweenEvent(start, end) {
  _active = defaultActive
  _value = if (active) defValue else defaultValue
}

/**
 * this Event class makes sure it's reactions are triggered before the ones of the
 * underlying event. Helps avoid bugs coming from indirect events in conjunction
 * with intervals. see Test cases indirect-within1 and indirect-within2
 * @author Michael
 *
 */
protected[events] class BeforeNode[T](ev : Event[T]) extends EventNode[T]{
	override def pullParents(id : Int) = ev.pullIsActivated(id)
	
	lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
			val newReacts = new ListBuffer[(() => Unit, Trace)]
			reactions(id,v,newReacts)
			reacts prependAll newReacts
		}
	
	override def deploy{
		ev += onEvt
	}
	
	override def undeploy {
		ev -= onEvt
	}
}

class ExecutionEvent[T] extends IntervalEvent[T] {

  def start: Event[T] = emptyevent//_start
  def end: Event[_] = emptyevent//_end

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


class ExistenceIntervalNode[T,U](list: VarList[T], 
		evf: T => IntervalEvent[U]) extends BetweenEvent[U](
				list.any((t:T) => evf(t).before) || 
					(list.elementAdded && (t => evf(t).active) map ((t:T) => evf(t).getValue)),
				list.any((t:T) => evf(t).after) || (list.elementRemoved && (t => evf(t).active) )){
	
	private var counter : Int = 0
	
	override def startCondition(v:U) = counter == 0
	override def endCondition(v:Any) = counter == 1
	
	override def deploy{
		start += (_ => counter += 1)
		end += (_ => counter -= 1)
		super.deploy
	}
}

