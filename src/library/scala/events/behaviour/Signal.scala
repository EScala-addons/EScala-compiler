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
    override def apply() = {
      currentDependents(stackEl)
    }

    /* DEBUGGING def's --> toRemove */
    def getLevel() = stackEl.level
    def getDep() = stackEl
    def getMyDeps() = myDeps.toList

	def currentDependents() = currentDependents(stackEl)
	
    def currentDependents(ownDep: Dependent) : T = {
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
			// TODO: refactor
            onChange(x, reeval)
          }
        }
        case None => //println("head == none")
      }
    }

    def getChanges(): List[T] = {
      val retArray = changes.toList
      retArray
    }
  }
}

trait Signal[T] extends Reactive[T]{
  lazy val stackEl = new Dependent()

  val changes = new ArrayStack[T]

  def checkDependents(dep: Dependent)
  def currentDependents(dep: Dependent): T

  // initial task on create
  checkDependents(stackEl)
}