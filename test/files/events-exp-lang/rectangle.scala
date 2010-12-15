import scala.events._

class Point(var x: Int, var y: Int){    
  // Moved event returns the new position
  evt moved[(Int,Int)] = afterExec(moveBy).map((_:Any) => (x,y))

  def moveBy(dx: Int, dy: Int) = {
    x += dx
    y += dy
  }
}

class Rectangle(val upperleft: Point, val lowerright: Point){
  // Resized reacts on movement of the lowerright corner.
  // This shows the new Syntax.
  // compiler error: type events.this.Event[...] does not take parameter
  evt resized(dx: Int, dy: Int) = lowerright.moved/*(dx,dy)*/

  // moved reacts on movement of the upperleft corner
  // this is the old syntax for comparison in the ast
  evt moved[(Int,Int)] = upperleft.moved

  def resizeBy(dx: Int, dy: Int) = {
    lowerright.moveBy(dx, dy)
  }
}

object Test {
  def main(args: Array[String]) {
    val point = new Point(5, 5)
    val rectangle = new Rectangle(new Point(3, 3), new Point(5, 5))

//  point.moved += pointMoved _
    point.moveBy(2, 2)
//  point.moved -= pointMoved _

//  rectangle.resized += rectangleResized _
    rectangle resizeBy(1, 2)
//  rectangle.resized -= rectangleResized _
  }

  def pointMoved() {
    println("point moved")
  }

  def rectangleResized() {
    println("rectangle resized")
  }
}
