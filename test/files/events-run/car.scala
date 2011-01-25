import scala.events._

class Car {
	
	imperative evt start[Unit]
	imperative evt stop[Unit]
	imperative evt gearUp[Unit]
	imperative evt gearDown[Unit]
	imperative evt accelerate[Int]
	imperative evt brake[Int]
	
	val started = between(start && (_ => gear == 0),stop || (brake && (_ => gear != 0) ))
	evt velocityChanged = 
		(accelerate && (_ > 0) || 
				(brake && (_ > 0) map ( (x :Int) => -x))
				) within started
		//this would be nicer if we could access the intervals value from within it conveniently somehow
	var velocity = 0
	velocityChanged += (velocity += _)
	var gear = 0
	gearUp += (_ => gear += 1)
	gearDown += (_ => gear -= 1)
	
	
	
	
}




object Test {
	def main(args: Array[String]){
		val car : Car = new Car
		car.start += (_ => println("Car started"))
		car.stop += (_ => println("Car stopped"))
		car.started.before += (_ => println("Car motor turned on"))
		car.started.after += (_ => println("Car motor turned off"))
		car.velocityChanged += (_ => println("Velocity is now " + car.velocity))
		car.gearUp || car.gearDown += (_ => println("Gear is now " + car.gear))
		
		
		car.gearUp()
		car.start()
		car.gearDown()
		car.start()
		car.accelerate(5)
		car.gearUp()
		car.accelerate(20)
		car.brake(10)
		car.gearUp()
		car.gearDown()
		car.gearDown()
		car.start()
		car.gearUp()
		car.accelerate(1000)
		car.gearUp()
		car.accelerate(0)
		car.gearUp()
		car.gearDown()
		car.gearDown()
		car.accelerate(30000000)
		car.stop()
		
		println("Driving at light speed in a stopped car!")
	}
}
