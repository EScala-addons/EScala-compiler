package tests.indirect
import scala.events._


object Test{

val ev1 = new ImperativeEvent[Unit]
val ev2 = new ImperativeEvent[Unit]
val ev3 = new ImperativeEvent[Unit]

val betweenEv = between(ev1,ev2)

ev1 += (_ => {println("EV1"); ev3(); ev2()})
ev2 += (_ => {println("EV2"); ev3()})
ev3 within betweenEv += (_ => println("Within"))
ev3 not_within betweenEv += (_ => println("Without"))

betweenEv.before += (_ => println("Before"))
betweenEv.after += (_ => println("After"))

def main(args: Array[String]){
 ev1()
 ev3()
 /*
  * this should print Before, Within, After, Without
  */

}


}