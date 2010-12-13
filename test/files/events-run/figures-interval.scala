t cimport scala.events._


class Figure {
	
	val changeOp = (execution(moveBy) || execution(resize)).map(getBounds _)
	evt boundsChanged = changeOp.after && (_ => changeOp.value != getBounds)
	evt areaChanged = boundsChanged && (_ => 
	changeOp.value._3 * changeOp.value._4 != w * h) map (_ => w*h)
	//this could also be expressed in terms of resize
	
	var x : Int = 0
	var y : Int = 0
	var w : Int = 0
	var h : Int = 0
	
	observable def moveBy(x: Int , y : Int) = {this.x += x ; this.y += y}
	observable def resize(w: Int, h: Int) = {this.h = h; this.w = w}
	def getBounds = (x,y,w,h)
}



object Test {
	
	def main(args : Array[String]){
		val fig = new Figure
		fig.x = 10
		fig.y = 10
		fig.w = 5
		fig.h = 20 
		boundsChanged += (b => println("Bounds changed to " + b))
		areaChanged += (a => println("Area changed to " + a))
		
		fig.moveBy(1,3)
		fig.moveBy(0,0)
		fig.moveBy(-2,-2)
		fig.resize(5,20)
		fig.resize(10,10)
		fig.resize(5,10)
		fig.moveBy(1,-1)
		
		
	}
}