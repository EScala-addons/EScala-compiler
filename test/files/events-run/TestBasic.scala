package micro
import scala.events._
import behaviour._
import java.text.SimpleDateFormat
import java.util.Date
/**
 * Testing Scenario
 *  Basic Dependency Creation
 *  (sum of referenced Vars)
 */

object TesterBasic {
	protected val tstampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS ")
    def tstamp = tstampFormat.format(new Date)
  
  def main(args: Array[String]) = {

	Console.err.println("STARTING TESTING")
    
	println("logging start: " + tstamp)
    
	val a = new Var(1)
    val b = new Var(2)
    val beh = Signal{ a() + b () }
	
	beh.changed += debugBeh _
	
	// new setter def -> TODO: remove def :=
	a() += 100 
	
    a() = 10
    
    a() = 20
    a() = 50
    a() = 100
    
    
    def debugBeh(newVal: Int) = {
      Console.err.println("got change! to: " + newVal)
    }
    
    val retList = beh.getChanges()
    println("debug beh.changes(history): " + retList.toString())
//    
    
    println("logging end: " + tstamp)
    
    Console.err.println("FINISHED TESTING")
  }

}


