import scala.events._

class Rectangle(){
  var x = 0;
  var y = 0;

  // moved reacts on movement of the upperleft corner
  // this is the old syntax for comparison in the ast
  evt moved[(Int,Int)] = afterExec(moveBy).map((_:Any) => (1,2))

  def moveBy(dx: Int, dy: Int) = {
    x += dx
    y += dy
  }

  evt change_order(x: Int, y: Int) = moved(y,x)
}

object Test {
  def main(args: Array[String]) {
    val rectangle = new Rectangle()
    
    rectangle.change_order += evtChangeMsg _
    rectangle moveBy(1,3)
    rectangle.change_order -= evtChangeMsg _
  }

  def evtChangeMsg(x: Int,y: Int) {
    println("changed order: by x: " + x + " y: " + y )
  }
}
