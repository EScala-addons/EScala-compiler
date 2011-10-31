package micro
import scala.events._
import behaviour._
import java.text.SimpleDateFormat
import java.util.Date

object Test {
	protected val tstampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS ")
    def tstamp = tstampFormat.format(new Date)
  
  def main(args: Array[String]) = {

	Console.err.println("STARTING TESTING")
    
//	println("logging start: " + tstamp)
    
	val a = new Var(1)
    val b = new Var(2)
    val beh = Signal{ a() + b () }
    val beh2 = Signal{ 10 + beh () }
    
    beh.changed += behChanged _
    beh2.changed += beh2Changed _
    
    a() = 3
    
    // obwohl changed event von beh2 noch nicht geworfen, 
    // wird bei Zugriff auf beh2 in beh1-observer der aktuelle wert evaluiert, 
    // daher kein Lack an korrekten Daten 
    def behChanged(newVal : Int) = {
      Console.err.println("beh changed: " + newVal)
      Console.err.println("testing beh2 before reeval: " + beh2())
    }
    
    def beh2Changed(newVal : Int) = {
      Console.err.println("beh2 changed: " + newVal)
    }

    
//    println("debug beh.changes(history): " + retList.toString())
    
//    println("logging end: " + tstamp)
    
    Console.err.println("FINISHED TESTING")
  }

}


