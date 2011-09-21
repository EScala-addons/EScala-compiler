package scala.events
package behaviour
import scala.collection.mutable._

object Signal {
  /*
   * Thread Local with ArrayStack[Dependent] holding:
   * Dependent represents corresponding CachingSignal
   */
  val dependentStack = {
    import scala.collection.mutable._
    val stack = new ArrayStack[Dependent]
    val tls = new ThreadLocal[ArrayStack[Dependent]]
    //    stack push Dependent.Nil
    tls set stack
    tls
  }

  /*
   * Handler for Stack:
   * if first-time connect or on notifying, use Stack for rechecking Dependents
   * gets dependent from calling def's, here on:
   * - init call (checkDependents -> creates new dep-instance)
   * - update call(def update: reaction to dep.invalidating evt, called by class changing which is setting param with dep)
   */
  def addToStack[T](dep: Dependent)(op: => T): T = {
    val stack = dependentStack.get
    stack push dep
    val cur = op
    stack.pop
    cur
  }

  def apply[T](op: => T) = {
    new CachingSignal(op)
  }

  class CachingSignal[T](op: => T) extends Signal[T] {

    /* if CachingSignal() called in beh2-expression, then check Stack for creating dependencies:
     * beh2 depends on beh1(this), i.e. if beh1 changes then beh2 must be reevaluated
     * Example: 
     * beh2 = Signal{ beh1() ....}
     */
    def apply() = {
      currentDependents(stackEl)
    }

    /* DEBUGGING def's --> toRemove */
    def getLevel() = stackEl.level
    def getDep() = stackEl
    def getMyDeps() = myDeps.toList

    def currentDependents(ownDep: Dependent) = {
      val curStack = Signal.dependentStack.get
      // dep ist der aktuell erstellte; ownDep ist der aufgerufene in expr-eval (aktuelle Sicht!):
      // wenn aufgerufenes Level > erstellte.level dann erstellte.level = aufgerufene.level + 1
      curStack.foreach(dep => dep.level = Math.max(ownDep.level + 1, dep.level))
      op
    }

    /*
	 * initial def when creating CachingSignal: 
	 * push beh.dep to stack and eval beh.expression for adding beh.dep 
	 * to depList of referenced reactives in expr. (var or signal)
	 */
    override def checkDependents(dep: Dependent) = {
      // TODO: generally needed, refactor location
      stackEl.invalidating += update _
      //      stackEl.collect += collectDeps _
      val current = addToStack(stackEl)(op)
      // track value of this CachingSignal -> push also init value in changes-arraystack
      changes.headOption match {
        case Some(x) => () //Console.err.println("on init checkDependents, head = " + x)
        case None => //println("on init head == none"); 
          changes push current
      }
    }

    def update(dep: Dependent) = {
      dep.resetLevel()
      val reeval = op
      changes.headOption match {
        case Some(x) => {
          addToStack(dep)(op)
          if (reeval != x) {
            changes push reeval
            changed(reeval)
            onChange(x, reeval)
          }
        }
        case None => //println("head == none")
      }
      invalidated(reeval)
    }

    def getChanges(): List[T] = {
      val retArray = changes.toList
      retArray
    }

    def flatten[U](implicit evf: T => Event[U]) = new EventNodeFlatten(this, evf)

    // TODO....
    def +=(reaction: => Unit) = {
      //      op match {
      //      	case evt : ImperativeEvent[(_, _)] => {
      ////      	  this.flatten += reaction _
      //      	  println("value of CachingSignal is Event!!!") 
      //      	}
      //      	case _ => {
      ////      				this.changes += reaction _
      //      				println("value of CachingSignal is something else!!!")
      //      			}
      //      }
    }
  }
}

trait Signal[T] {
  //  Console.err.println("constructor TRAIT Signal")
  lazy val stackEl = new Dependent()

  val changes = new ArrayStack[T]
  var myDeps = new ArrayStack[Dependent]

  def checkDependents(dep: Dependent)

  def currentDependents(dep: Dependent): T

  // extract in active.scala
  lazy val onChange = new ImperativeEvent[(T,T)]
  lazy val changed = new ImperativeEvent[T]
  // event for permeable Signal, not checking if changed, firing everytime CachingSignal is reevaluated
  lazy val invalidated = new ImperativeEvent[T]

  // initial task on create
  checkDependents(stackEl)
}

/*
 * Implements reference to an event of an object (referenced by a CachingSignal)
 */
class EventNodeFlatten[T, U](target: Signal.CachingSignal[T], evf: T => Event[U]) extends EventNode[U] {

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