import scala.events._


abstract class Alarm {
	
<<<<<<< HEAD
	val isArmed = between(afterExec(arm),afterExec(disarm)) // \ isAlarmed
	//Recursive Definition of Events is impossible (isn't it?)
	imperative evt intrusionDetected[Unit]
	evt alarm[Unit] = intrusionDetected within isArmed
	val isAlarmed : IntervalEvent[Unit,Unit] = between(alarm,afterExec(disarm))
=======
	val isArmed = between(afterExec(arm),afterExec(disarm)) \ isAlarmed
	imperative evt intrusionDetected[Unit]
	evt alarm = intrusionDetected within isArmed
	val isAlarmed = between(alarm,afterExec(disarm))
>>>>>>> Alarm und Figure Beispiel
	
	def arm() : Unit
	def disarm() : Unit
	
}


class TrapAlarm extends Alarm {
	
	evt trapped = intrusionDetected strictlyWithin isAlarmed
	
	def arm = { println("Trap armed") }
	def disarm = { println("Trap disarmed") }
	
}





object Test {
	def main(args: Array[String]){
		val trap = new TrapAlarm
		
		trap.alarm += (_ => println("Riiing Alarm"))
		trap.trapped += (_ => println("Intruder trapped"))
		
		trap.arm
		trap.disarm
		trap.arm
		trap.intrusionDetected()
		trap.intrusionDetected()
		trap.intrusionDetected()
		trap.disarm
		trap.arm
		
	}
	
}