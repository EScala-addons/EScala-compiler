package scala.events
package behaviour
import scala.events._

/**
 * Definition of Dependent:
 * represents a behaviour
 * has level in dependencyGraph
 * communicates with associated Behaviour beh and Changing-/Behaviour-references in beh-expression eval
 * requests beh for update
 * requests referenced Changings for removing out of Changing.myDeps
 */

object Dependent {
  case object Nil extends Dependent {  }
  def apply() = new Dependent()
}

class Dependent extends Ordered[Dependent] {
//class Dependent {
  var level : Int = -1
  
  def resetLevel() = {
    level = -1
  }
  
  def compare(that : Dependent) = {
    //that.level - this.level
    this.level - that.level
  }
  // Event reacting on Changing-update:
  lazy val invalidating = new ImperativeEvent[Dependent]
 
  lazy val collect = new ImperativeEvent[Unit]
}
