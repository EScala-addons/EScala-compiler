package scala.events

import scala.collection.mutable.{ ListBuffer, Stack }
import scala.util.DynamicVariable

trait Event[+T] {

  /**
   * Sink function type: takes event id, event parameter
   * fills the given list with reactions to be executed
   * the reactions represt
   */
  type Sink = (Int, T, ListBuffer[(() => Unit, Trace)]) => Unit
  type Trace = List[Event[_]]

  /**
   * Register a sink function to the event (used for propagation of events)
   */
  protected[events] def +=(sink: Sink)

  /**
   * Unregister a sink function
   */
  protected[events] def -=(sink: Sink)

  /**
   * Register a reaction to the event
   */
  def +=(react: T => Unit)

  /**
   * Unregister a reaction
   */
  def -=(react: T => Unit)

  /**
   * Events disjunction.
   */
  def ||[S >: T, U <: S](other: Event[U]) = new EventNodeOr[S](this, other)

  /**
   * Creates an events sequence
   */
  def then[U, V, S >: T](other: => Event[U], merge: (S, U) => V) = new EventNodeSequence[S, U, V](this, other, merge)

  /**
   * Creates an events sequence with a merge method creating a tuple of both event parameters
   */
  def then[U, S >: T](other: => Event[U]) = new EventNodeSequence[S, U, (S, U)](this, other, (p1: S, p2: U) => (p1, p2))

  /**
   * Event filtered with a predicate
   */
  def &&[U >: T, V >: T <: U](pred: U => Boolean) = new EventNodeFilter[V](this, pred)

  /**
   * Event filtered with a boolean variable
   */
  def &&[U >: T](pred: () => Boolean) = new EventNodeFilter[U](this, _ => pred())

  /**
   * Event is triggered except if the other one is triggered
   */
  def \[U >: T](other: Event[_]) = new EventNodeExcept[U](this, other)

  /**
   * Events conjunction
   */
  def and[U, V, S >: T](other: Event[U], merge: (S, U) => V) = new EventNodeAnd[S, U, V](this, other, merge)

  /**
   * Event conjunction with a merge method creating a tuple of both event parameters
   */
  def and[U, S >: T](other: Event[U]) = new EventNodeAnd[S, U, (S, U)](this, other, (p1: S, p2: U) => (p1, p2))

  /**
   * Transform the event parameter
   */
  def map[U, S >: T, V >: S](mapping: V => U) = new EventNodeMap[S, U](this, mapping)

  /**
   * Drop the event parameter; equivalent to map((_: Any) => ())
   */
  def dropParam[S >: T] = new EventNodeMap[S, Unit](this, _ => ())

  /**
   * Event inside an Interval
   */
  def within(ie: IntervalEvent[_]) : Event[T] = events.within(this, ie, (t:T,_:Any) => t)

  /**
   * Event inside an Interval. This method gives the possibility to access the 
   * interval's value
   * @param ie
   * @param merge function that processes this event's value and the Interval's value
   * @return
   */
  def within[S,U](ie: IntervalEvent[S], merge: (T,S) => U): Event[U] = events.within(this,ie,merge)
  /**
   * Event outside an Interval
   */
  def not_within(ie: IntervalEvent[_]) = events.not_within(this, ie)
  /**
   * Event strictly within an Interval, meaning except it's beginning and end
   */
  def strictlyWithin(ie: IntervalEvent[_]) = events.strictlyWithin(this, ie)
  /**
   * Event without an Interval, but including it's beginning and end
   */
  def not_strictlyWithin(ie: IntervalEvent[_]) = events.not_strictlyWithin(this, ie)

  /**
   * pull whether the event has been triggered
   * @param id the current event id; Note that behavior is undefined if id != EventIds.lastId
   * @return
   */
  protected[events] def pullIsActivated(id: Int): Option[T]
}

/*
 *  Base class for events.
 *  Maintains sinks and reactions and
 */
abstract class EventNode[T] extends Event[T] {
  protected val sinks = new ListBuffer[Sink]
  protected[events] val _reactions = new ListBuffer[T => Unit]

  /*
   * Register a reaction to the event
   */
  def +=(sink: Sink) {
    sinks += sink
    // deploy the event if the first reaction/sink is registered
    if (sinks.size == 1 && _reactions.size == 0)
      deploy
  }

  def -=(sink: Sink) {
    sinks -= sink
    // undeploy the event if the last reaction/sink is unregistered
    // has cascade effect
    // avoids redundant event propagation
    if (sinks.size == 0 && _reactions.size == 0)
      undeploy
  }

  /*
   * Register a reaction to the event
   */
  def +=(react: T => Unit) {
    _reactions += react
    // deploy the event if the first reaction/sink is registered
    if (_reactions.size == 1 && sinks.size == 0)
      deploy
  }

  /*
   * Unregister a reaction
   */
  def -=(react: T => Unit) {
    _reactions -= react
    // undeploy the event if the last reaction/sink is unregistered
    if (sinks.size == 0 && _reactions.size == 0)
      undeploy
  }

  /**
   * Deploy the event (i.e. registers to the referenced events)
   */
  protected def deploy: Unit

  /**
   * Undeploy the event (i.e. unregisters from the referenced events)
   */
  protected def undeploy: Unit

  /** Collects the reactions registered with this event and associates the current event trace.
   *  It then propagates to sinks.
   */
  protected[events] def reactions(id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) {
    lastPulledId = id
    wasLastActivated = true
    lastValue = v
    eventTrace.withValue(this :: eventTrace.value) {
      // collect the reactions registered with this event
      _reactions.foreach(react => reacts += ((() => react(v)) -> eventTrace.value))
      // propagate to sinks adding this event to the event trace
      sinks.foreach(sink => sink(id, v, reacts))
    }
  }

  protected var lastPulledId: Int = -1
  protected var wasLastActivated: Boolean = false
  protected var lastValue: T = _
  /**
   * helper function for pulling the parent's state, override this
   * to adapt the pull-behavior
   * @return <ul><li>None if the parents are not activated (== this one neither)</li>
   * 			<li>Some(v) otherwise, where v is the value that would get passed to
   * 			reactions 
   */
  protected def pullFkt(Id: Int): Option[T]
  
  protected[events] override final def pullIsActivated(Id: Int): Option[T] = {
    if (wasLastActivated && Id == lastPulledId) Some(lastValue)
    if(!wasLastActivated && Id == lastPulledId) None
    else {
      pullFkt(Id) match {
        case None => {
          lastPulledId = Id
          wasLastActivated = false
          return None
        }
        case Some(v) => {
          lastPulledId = Id
          wasLastActivated = true
          lastValue = v
          return Some(v)
        }
      }
    }
  }

}

protected[events] object eventTrace extends DynamicVariable[List[Event[_]]](Nil)

object EventIds {
  // last used event id
  var lastId: Int = 0

  /*
  * generate new event id
  */
  def newId(): Int = {
    lastId += 1
    lastId
  }
}

/*
 * An implementation of an explicit event
 */
class ImperativeEvent[T] extends EventNode[T] {

  private var deployed = false

  /*
  * Trigger the event
  */
  def apply(v: T) = {
    beforeTrigger(v)
    // does something only if the event is deployed, i.e. if some reactions or sinks
    // are registered
    if (deployed) {
      // collect matching reactions
      val reacts: ListBuffer[(() => Unit, Trace)] = new ListBuffer

      reactions(EventIds.newId(), v, reacts)
      // once reactions are collected, we are after the triggering
      afterTrigger(v)
      // execute the collected reactions
      reacts.foreach(
        react => {
          eventTrace.withValue(react._2) {
            //try {
            react._1()
            /*} catch {
              case e => 
                println("Event trace:")
                println(eventTrace.value.mkString("", "\n", "\n"))
                throw e
            }*/
          }
        })
    } else {
      afterTrigger(v)
    }
  }

  protected[events] def beforeTrigger(v: T) {}
  protected[events] def afterTrigger(v: T) {}

  protected override def deploy { deployed = true }

  protected override def undeploy { deployed = false }

  protected override def pullFkt(Id: Int): Option[T] = {
    if (wasLastActivated && lastPulledId == Id)
      Some(lastValue)
    else None
  }

  override def toString = getClass.getName

}

/*
 * Implementation of event conjunction
 */
class EventNodeAnd[T1, T2, T](ev1: Event[T1], ev2: Event[T2], merge: (T1, T2) => T) extends EventNode[T] {

  // The id of the last received event
  var id: Int = 0

  // Parameter value of event1 or event2 (depending on which is received first)
  var v1: T1 = _
  var v2: T2 = _
  var currentTrace: Trace = Nil

  /*
  * Reaction to event1
  */
  lazy val onEvt1 = (id: Int, v1: T1, reacts: ListBuffer[(() => Unit, Trace)]) => {
    if (this.id == id) {
      // event2 is already received; collect the reactions
      eventTrace.withValue(currentTrace ::: eventTrace.value) {
        reactions(id, merge(v1, this.v2), reacts)
      }
    } else {
      // event2 is not received yet; save the data of the event1
      this.id = id
      this.v1 = v1
      this.currentTrace = eventTrace.value
    }
  }

  /*
   * Reaction to event2
   */
  lazy val onEvt2 = (id: Int, v2: T2, reacts: ListBuffer[(() => Unit, Trace)]) => {
    if (this.id == id) {
      // event1 is already received; collect the reactions
      eventTrace.withValue(currentTrace ::: eventTrace.value) {
        reactions(id, merge(this.v1, v2), reacts)
      }
    } else {
      // event1 is not received yet; save the data of the event2
      this.id = id
      this.v2 = v2
      this.currentTrace = eventTrace.value
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev1 += onEvt1
    ev2 += onEvt2

  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev1 -= onEvt1
    ev2 -= onEvt2

  }

  protected override def pullFkt(Id: Int): Option[T] = {

    val v1 = ev1.pullIsActivated(Id)
    if (v1 == None) return None
    val v2 = ev2.pullIsActivated(Id)
    if (v2 == None) return None
    return Some(merge(v1.get, v2.get))

  }

  override def toString = "(" + ev1 + " and " + ev2 + ")"

}

/*
 * Implementation of event disjunction
 */
class EventNodeOr[T](ev1: Event[_ <: T], ev2: Event[_ <: T]) extends EventNode[T] {

  // The id of the last received event
  var id: Int = 0

  /*
   * Reaction to both events
   */
  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // if the event was already received, avoid processing it twice
    if (this.id != id) {
      this.id = id
      // the event received for the first time; collect the reactions
      reactions(id, v, reacts)
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev1 += onEvt
    ev2 += onEvt

  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev1 -= onEvt
    ev2 -= onEvt
  }

  override def toString = "(" + ev1 + " || " + ev2 + ")"

  protected override def pullFkt(Id: Int): Option[T] = {
    val v1 = ev1.pullIsActivated(Id)
    if (v1 != None) return v1
    val v2 = ev2.pullIsActivated(Id)
    if (v2 != None) return v2
    None
  }

}

/*
 * Implements transformation of event parameter
 */
class EventNodeMap[T, U](ev: Event[T], f: T => U) extends EventNode[U] {

  /*
   * Reaction to the referenced event
   */
  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // transform v to f(v)
    reactions(id, f(v), reacts)
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev += onEvt
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev -= onEvt
  }

  protected override def pullFkt(Id: Int): Option[U] = {
    ev.pullIsActivated(Id) match {
      case None => None
      case Some(v) => Some(f(v))
    }
  }

  override def toString = getClass.getName

}

/*
 * Implements filtering event by a predicate
 */
class EventNodeFilter[T](ev: Event[T], f: T => Boolean) extends EventNode[T] {

  /*
   * Reaction to the referenced event
   */
  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // filter the event by f(v)
    if (f(v)) {
      reactions(id, v, reacts)
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev += onEvt
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev -= onEvt
  }

  protected override def pullFkt(Id: Int): Option[T] = {
    ev.pullIsActivated(Id) match {
      case None => None
      case Some(v) =>
        if (f(v)) {
          Some(v)
        } else None
    }
  }

  override def toString = "(" + ev + " && <predicate>)"

}

/*
 * Implements reference to an event of an object (referenced by a variable)
 */
class EventNodeRef[T, U](target: Variable[T], evf: T => Event[U]) extends EventNode[U] {

  /*
  * Currently referenced event
  */
  private var ev: Event[U] = if (target.value != null) evf(target.value) else emptyevent

  import EventsLibConversions._

  /*
   * Reaction to a change of the target
   */
  lazy val onTargetChanged = toTupledFun2((oldTarget: T, newTarget: T) => {
    // unregister from the current event
    ev -= onEvt
    // retrieve and save the new event
    if (newTarget != null)
      ev = evf(newTarget)
    else
      ev = emptyevent
    // register to the new event
    ev += onEvt
  })

  /*
   * Reaction to the currently referenced event
   */
  lazy val onEvt = (id: Int, v: U, reacts: ListBuffer[(() => Unit, Trace)]) => {
    reactions(id, v, reacts)
  }

  /*
  * Register to the referenced event and changes of the target
  */
  protected override def deploy {
    ev += onEvt
    target.changed += onTargetChanged
  }

  /*
  * Unregister from the referenced event and changes of the target
  */
  protected override def undeploy {
    ev -= onEvt
    target.changed -= onTargetChanged
  }

  protected override def pullFkt(Id: Int): Option[U] = ev.pullIsActivated(Id)

  override def toString = getClass.getName

}

/*
 * Implementation of quantification over a (varying) list of objects
 */
class EventNodeExists[T, U](list: VarList[T], evf: T => Event[U]) extends EventNode[U] {

  /*
   * Register to the event of a newly added list element
   */
  lazy val onElementAdded = (target: T) => {
    evf(target) += onEvt
  }

  /*
   * Unregister from the event of a removed list element
   */
  lazy val onElementRemoved = (target: T) => {
    evf(target) -= onEvt
  }

  /*
   * Reaction to the observed events
   */
  lazy val onEvt = (id: Int, v: U, reacts: ListBuffer[(() => Unit, Trace)]) => {
    reactions(id, v, reacts)
  }

  /*
  * Register to the events of all list elements and the list changes
  */
  protected override def deploy {
    list.foreach(target => evf(target) += onEvt)
    list.elementAdded += onElementAdded
    list.elementRemoved += onElementRemoved
  }

  /*
  * Unregister from the events of all list elements and the list changes
  */
  protected override def undeploy {
    list.foreach(target => evf(target) -= onEvt)
    list.elementAdded -= onElementAdded
    list.elementRemoved -= onElementRemoved
  }

  protected override def pullFkt(Id: Int): Option[U] = {
    list.foreach(target => evf(target).pullIsActivated(Id) match {
      case Some(v) => return Some(v)
      case None =>
    })
    None
  }

  override def toString = getClass.getName

}

/*
 * Implementation of event sequence operator
 */
class EventNodeSequence[T, U, V](ev1: Event[T], ev2: => Event[U], merge: (T, U) => V) extends EventNode[V] {

  // the id of the last received event1
  // -1 if event1 was not received yet (or after last event2)
  var id: Int = -1
  // value of the last event1
  var v1: T = _

  /*
  * Reaction to event1
  */
  lazy val onEvt1 = (id: Int, v1: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // ignore consecutive occurrences of event1
    if (this.id == -1) {
      // save the data of event1
      this.id = id
      this.v1 = v1
    }
  }

  /*
   * Reaction to event2
   */
  lazy val onEvt2 = (id: Int, v2: U, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // react to event2 only if event1 was already received;
    // also ensure that event2 is different from event1 by comparing the ids
    if (this.id != -1 && this.id != id) {
      // collect the reactions
      reactions(id, merge(this.v1, v2), reacts)
      // reset the sequence
      this.id = -1
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev1 += onEvt1
    ev2 += onEvt2
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev1 -= onEvt1
    ev2 -= onEvt2
  }

  protected override def pullFkt(Id: Int): Option[V] = {
    if (this.id != -1 && this.id != Id) {
      ev2.pullIsActivated(Id) match {
        case Some(v) => return Some(merge(this.v1, v))
        case None =>
      }
    }
    None
  }

  override def toString = "(" + ev1 + " then " + ev2 + ")"

}

class EventNodeCond[T](event: => Event[T]) extends EventNode[T] {

  lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    reactions(id, v, reacts)
  }

  private def getEvent(ev: => Event[T]): Event[T] =
    try {
      event
    } catch {
      case _: NullPointerException => emptyevent
    }

  override def deploy {
    getEvent(event) += onEvt
  }
  override def undeploy {
    getEvent(event) -= onEvt
  }

  protected override def pullFkt(Id: Int): Option[T] = getEvent(event).pullIsActivated(Id)

}

class EventNodeExcept[T](accepted: Event[T], except: Event[_]) extends EventNode[T] {

  lazy val onAccepted = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    if (pullIsActivated(id) != None) {
      reactions(id, v, reacts)
    }
  }

  lazy val onExcept = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
    // the except event is received, set the id to
    lastPulledId = id
    wasLastActivated = false
  }

  override def deploy {
    except += onExcept
    accepted += onAccepted
  }

  override def undeploy {
    accepted -= onAccepted
    except -= onExcept
  }

  protected override def pullFkt(Id: Int): Option[T] = {
    if (except.pullIsActivated(Id) == None)
      accepted.pullIsActivated(Id) else None
  }

}

class CausedByFilter(e: Event[_]) extends Function0[Boolean] {
  def apply() = eventTrace.value.contains(e)
  def unary_! = () => !eventTrace.value.contains(e)
}

/*
 * Implementation of an observable variable
 */
class Variable[T](private var v: T) {
  def value: T = this.v

  def value_=(v: T) = {
    if (this.v != v) {
      val old = this.v
      this.v = v
      changed(old, v)
    }
  }

  def :=(v: T) = value_=(v)
  def apply(): T = value

  lazy val changed = new ImperativeEvent[(T, T)]

  /*
   * A convenience operator for referencing an event of the variable
   */
  def event[U](evf: T => Event[U]) = new EventNodeRef(this, evf)
}

object Variable {
  def apply[T](v: T) = new Variable(v)
  def unapply[T](v: Variable[T]): Option[T] = Some(v())
}

/*
 * Implementation of an observable list
 */
class VarList[T]() extends Iterable[T] {
  // Use list buffer for implementation
  private val buffer = new ListBuffer[T]

  // delegate the iterator implementation
  override def iterator = buffer.iterator

  /*
   * Add a new element to the list; trigger the corresponding event
   */
  def +=(v: T) = { buffer += v; elementAdded(v) }

  /*
  * Remove an element from the list; trigger the corresponding event
  */
  def -=(v: T) = { buffer -= v; elementRemoved(v) }

  def clear() = {
    buffer.foreach(v => elementRemoved(v))
    buffer.clear()
  }

  /*
   * A convenience operator creating an event based on the list
   */
  def any[U](evf: T => Event[U]) = new EventNodeExists(this, evf)

  def any[U](evf: T => IntervalEvent[U]) = new ExistenceIntervalNode[T, U](this, evf)

  /*
  * Events notifying over the changes in the list
  */
  lazy val elementAdded = new ImperativeEvent[T]
  lazy val elementRemoved = new ImperativeEvent[T]
}

object VarList {
  def apply[T](a: Array[T]) = {
    val result = new VarList[T]()
    a.foreach(v => result += v)
    result
  }
  def apply[T](values: Iterable[T]) = {
    val result = new VarList[T]()
    values.foreach(v => result += v)
    result
  }
}

/*
 * Implicit type conversions
 */
object EventsLibConversions {
  // implicitly to an observable variable
  implicit def toVariable[T](o: T) = Variable(o)
  // implicit or explicit lifting of methods for instrumentation
  implicit def lift[T, U](body: T => U) = Observable(body)
  // implicitly drop event parameter if not used
  implicit def dropParam[T](ev: Event[T]) = ev.dropParam

  // some implicit conversion for methods which allow to write 
  // instrumented methods in a more intuitive way.
  implicit def toUnitfun[T](f: () => T) = (_: Unit) => f()
  implicit def toTupledFun2[T1, T2, R](f: (T1, T2) => R) = f.tupled
  implicit def toTupledFun3[T1, T2, T3, R](f: (T1, T2, T3) => R) = f.tupled
  implicit def toTupledFun4[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R) = f.tupled
  implicit def toTupledFun5[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R) = f.tupled
  implicit def toTupledFun6[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R) = f.tupled
  implicit def toTupledFun7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R) = f.tupled
  implicit def toTupledFun8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = f.tupled
  implicit def toTupledFun9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = f.tupled
  implicit def toTupledFun10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = f.tupled
}

/*
 * Implementation of an observable method
 */
class Observable[T, U](body: T => U) extends (T => U) {
  // before and after, modeled as primitive events
  lazy val before = new ImperativeEvent[T]
  lazy val after = new ImperativeEvent[(T, U)]

  /*
  * Instrumented method implementation:
  * trigger events before and after the actual method execution
  */
  def apply(t: T): U = {
    before(t)
    val res = body(t)
    after(t, res)
    res
  }
}

object Observable {
  def apply[T, U](f: T => U) = new Observable(f)
}

