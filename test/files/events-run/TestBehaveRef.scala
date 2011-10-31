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

object Test {
	protected val tstampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS ")
    def tstamp = tstampFormat.format(new Date)
  
  def main(args: Array[String]) = {

	Console.err.println("STARTING TESTING")
    
	println("logging start: " + tstamp)
	
	val a = new Var(1)
	val b = new Var(2)
	val x = new Var(true)
	
	val switch = Signal{ if(x()) a() else b() }
	


	println("init state: switch = ( if (x) a else b)")
	println("a my deps (must be 1): " + a.getMyDeps())
	println("b my deps (must be 0): " + b.getMyDeps())
	
	x() = false
	
	println("changed x!")
	println("a my deps (must be 1): " + a.getMyDeps())
	println("b my deps (must be 1): " + b.getMyDeps())
	
	b() = 20
	
	println("changed b!")
	println("a my deps (must be 1): " + a.getMyDeps())
	println("b my deps (must be 1): " + b.getMyDeps())
	
	a() = 100
	
	println("changed a!")
	println("a my deps (must be 0): " + a.getMyDeps())
	println("b my deps (must be 1): " + b.getMyDeps())
	
	
	
	
    
	  
    println("logging end: " + tstamp)
    
    Console.err.println("FINISHED TESTING")
  }

}


