import scala.events._

object Test{

imperative evt ev[Unit]

val variable1 : Variable[Event[_]] = ev || emptyevent
val variable2 : Variable[Event[_]] = ev || emptyevent
val variable3 : Variable[Event[_]] = ev || emptyevent

evt buggy = variable1.event(evnt => evnt) and (variable2.event(evnt => evnt) \ variable3.event(evnt => evnt))

buggy += (_ => println("Bug"))

//magic, change the order of the reactions in an unfavorable manner
variable3 := variable2()
variable1 := variable2()

def main(args: Array[String]){
 ev() //as buggy can never happen, this should not cause a reaction


}


}