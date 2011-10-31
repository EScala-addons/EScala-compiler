package scala.events
package behaviour
import scala.collection.mutable._
import java.io._

/**
 * Definiton of Var value Var:
 * represents value that changes over time
 * can be called in behaviour expressions:
 * 	-> keeps track of referencing behaviours with ArrayStack
 * 	-> requests behaviours (via behaviour.dep) to update if value of this changed
 * 	-> dynamically removes Dependent dep if changes of this has no effect on dep
 * 		-> unregister event-reaction connection between this and dep -> for garbageCollecting
 * 		  (requested by dep after behaviour update )
 */

object Var {
  val level = 0

  def apply[T](init: T) = new Var(init)
}

class Var[T](init: T) extends Reactive[T] {
  var _value: T = init

  /**
   * def apply:
   * getter for current value
   * @return currentValue : T
   */
  override def apply() = {
    currentDependents()
    _value
  }

  /**
   * def update:
   * set new value of time-varying this
   * on dif to current value:
   * 	-> calls dependents for automated data synch
   * 	-> causes event-throwing for calling observers on this  
   */
  def update(newVal: T) = 
    if (_value != newVal) {
      val oldVal = _value
      // 1: set new Val
      _value = newVal
      // 2: notify Dependents for data synch
      // if (!myDeps.isEmpty) {
	  try {
		  if (!myDeps.isEmpty) {
			// println("myDeps: " + myDeps)
			notifyDependents()
		  }
	  }
	  catch {
		case np : NullPointerException => {
			// println("NullPointerCatched!")
			// import java.io.FileWriter
			// import scala.compat.Platform
			// val writer = new FileWriter("C:/DEV/Scala/git/animals/signal-modules/logs/nullPointers.txt", false)
			// writer.write("NullPointerCatched" + Platform.EOL)
		}
	  }
      // 3: call observers on this Var by throwing changed event
      changed(newVal)
	  onChange(oldVal, newVal)
    }

  /**
   * def currentDependents
   * called on apply in this.
   * if this is called inside of signal-expression then:
   * 	- Dependent.level updated if this is first reference in signal-expression
   * 	- Dependent of signal-expression owner is added to depList of this
   */
  def currentDependents() : T = {
    val curStack = Signal.dependentStack.get
    curStack.foreach(dep =>
      {
        if (!myDeps.contains(dep)) {
          if (dep.level < 0)
            dep.level = 1
          myDeps push dep
        }
      })
     _value
  }

  /**
   * def notifyDependents
   * routine for calling dependents in depList of this
   * 	- ordered call by lowest level first
   * 	- clearing depList of this for reeval of depList
   */
  def notifyDependents() = {
    val upStack = myDeps.clone()
    // clear depList of this:
    myDeps.clear()
    
    import java.util.{ PriorityQueue, Comparator }
    // create PriorityQueue for ordered call of deps in depList of this
    val myQueue = new PriorityQueue[Dependent](10, new Comparator[Dependent] {
      def compare(x: Dependent, y: Dependent): Int = x.level - y.level
    })
    
    // add all deps that need to be synchronized
    upStack.foreach(dep => {
      myQueue add dep
    })
    
    // Console.err.println("## Propagating.....")
    // ordered call of deps in depList of this by calling evt-invalidating of dep 
    // -> update-reaction of corresponding behaviour registered
    while (myQueue.size != 0) {
      val dep = myQueue.poll
      dep.invalidating(dep)
    }
    // Console.err.println("## Propagation finished")
  }
  
  // DEBUGGING DEF: TO REMOVE because internal data
  def getMyDeps() = myDeps.toList

}