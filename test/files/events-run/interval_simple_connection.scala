import scala.events._
import java.net._

/**
 * The State machine for a simple FileServerListener which accepts connections from
 * Clients if the Server was started and not stopped. 
 *
 */
class EventDrivenFileServer {
	
	val start = new ImperativeEvent[Unit]
	val stop = new ImperativeEvent[Unit]
	val running = new BetweenEvent[Unit](start, stop)
	val tryConnect = new ImperativeEvent[Unit]
	
	private val acceptConnection = tryConnect within running
	private val rejectConnection = tryConnect.not_within(running)
	
	start += {(u : Unit) => println("Start waiting for Connections")}
	stop += {(u: Unit) => println("Stopped the File Server")}
	tryConnect  += {(u : Unit) => println("Trying to connect")}
	acceptConnection += {(u: Unit) => println("Accepted Connection")}
	rejectConnection += {(u: Any) => println("Rejected the Connection")}
}

/**
 * The same behaviour like the example above. But the code is written a little bit 
 * more explicit that the above example. Therefor we use two interval events - one
 * for each state. The states are connected via the after-Events of the interval state before
 *
 */
class IntervallEventDrivenFileServer {
	val initEvent = new ImperativeEvent[Unit]
	val start = new ImperativeEvent[Unit]
	val stop = new ImperativeEvent[Unit]
	val tryConnect = new ImperativeEvent[Unit]
	
	initEvent()
	
	private lazy val startInterval = new BetweenEvent[Unit](stopInterval.after, stop)
	private lazy val stopInterval : BetweenEvent[Unit] = new BetweenEvent[Unit](startInterval.after || initEvent, start)
	
	private val rejectConnection = tryConnect within startInterval
	private val acceptConnection = tryConnect within stopInterval
	
	start += {(u : Unit) => println("Start waiting for Connections")}
	stop += {(u: Unit) => println("Stopped the File Server")}
	tryConnect  += {(u : Unit) => println("Trying to connect")}
	acceptConnection += {(u: Unit) => println("Accepted Connection")}
	rejectConnection += {(u: Any) => println("Rejected the Connection")}
}

/***
 * 
 * This is the state based implementation of the file Server
 * As you can see, it is longer and not as that intuitive readable
 */
class StateDrivenFileServer {
	
	var started = false
	var stopped = false
	
	def start {
		started = true
		println("Start waiting for Connections")
	}
	
	def stop {
		started = false
		stopped = true
		println("Stopped the File Server")
	}
	
	def tryConnect {
		println("Trying to connect")
		
		if(started && !stopped){
			acceptConnection
		}
		else {
			rejectConnection
		}
	}
	
	private def acceptConnection {
		println("Accepted connection")
	}
	
	private def rejectConnection {
		println("Rejected connection")
	}
}

object SimpleFileServer {
  def main(args : Array[String]) : Unit = 
  {
	  println("StartServer")
	  
	  var eventServer = new EventDrivenFileServer
	  
	  eventServer.tryConnect()
	  
	  eventServer.start()
	  eventServer.tryConnect()
	  eventServer.stop()
	  
	  eventServer.tryConnect()
	  
	  
	  var statedServer = new StateDrivenFileServer
	  
	  statedServer.tryConnect
	  
	  statedServer.start
	  statedServer.tryConnect
	  statedServer.stop
	  
	  statedServer.tryConnect
  }
}
