package scala.events
package behaviour
import scala.collection.mutable._



trait Reactive[T] {
  val myDeps = new ArrayStack[Dependent]

  /**
   * create Dependencies by checking Stack for dependents
   *  
   */
  def currentDependents() : T
  
  def apply() = currentDependents()
  
  def flatten[U](implicit evf: T => Event[U]) = new EventNodeFlatten(this, evf)
  
  /* changed Event thrown on value change */
  lazy val onChange = new ImperativeEvent[(T,T)]
  lazy val changed = new ImperativeEvent[T]
}

/*
 * Implements reference to an event of an object (referenced by a CachingSignal)
 */
class EventNodeFlatten[T, U](target: Reactive[T], evf: T => Event[U]) extends EventNode[U] {

  /*
  * Currently referenced event
  */
  private var ev: Event[U] = if (target() != null) evf(target()) else emptyevent

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
    target.onChange += onTargetChanged
  }

  /*
  * Unregister from the referenced event and changes of the target
  */
  protected override def undeploy {
    ev -= onEvt
    target.onChange -= onTargetChanged
  }

  override def toString = getClass.getName

}