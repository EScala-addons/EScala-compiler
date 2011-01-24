import scala.events._

/*class Point(var x: Int, var y: Int){    
  // Moved event returns the new position
  evt moved[(Int,Int)] = afterExec(moveBy).map((_:Any) => (x,y))

  def moveBy(dx: Int, dy: Int) = {
    x += dx
    y += dy
  }
}*/

class Rectangle(/*val upperleft: Point, val lowerright: Point*/){
  var x = 0;
  var y = 0;

  // moved reacts on movement of the upperleft corner
  // this is the old syntax for comparison in the ast
  evt moved[(Int,Int)] = afterExec(moveBy).map((_:Any) => (1,2))
// emptyevent //upperleft.moved

//  evt moved2[(Int,Int)] = moved

  def moveBy(dx: Int, dy: Int) = {
    x += dx
    y += dy
  }
  // Resized reacts on movement of the lowerright corner.
  // This shows the new Syntax.
  // compiler error: type events.this.Event[...] does not take parameter
//  evt resized(dx: Int, dy: Int)  = emptyevent //lowerright.moved(dx,dy)

  evt normal(a: Int, b: Int) = moved(a,b)
//  evt changes(x: Int, y: Int) = moved(y,x)
//  evt drop(x: Int) = moved(x,_)

//  evt normal_old(x: Int, y: Int) = moved.map((a: Int,b: Int)=>(a,b))(x,y)

//  evt inverted(x: Int, y: Int) = moved(y,x)
//  evt inverted_old(x: Int, y: Int) = moved.map((a: Int,b: Int)=>(a,b))

//  evt drop(x: Int) = moved(x,_)
//  evt drop_old(x: Int) = moved.map((a: Int,_)=>(b))
  

  /*def resizeBy(dx: Int, dy: Int) = {
    lowerright.moveBy(dx, dy)
  }*/
}

object Test {
  def main(args: Array[String]) {
//    val point = new Point(5, 5)
    val rectangle = new Rectangle(/*new Point(3, 3), new Point(5, 5)*/)

//  point.moved += pointMoved _
//    point.moveBy(2, 2)
//  point.moved -= pointMoved _

//  rectangle.resized += rectangleResized _
//    rectangle resizeBy(1, 2)
//  rectangle.resized -= rectangleResized _
    
    rectangle.normal += rectangleMoved _
    rectangle.changes += rectangleMoved _
    rectangle.normal_old += rectangleMoved2 _
    rectangle moveBy(1,3)
    rectangle.normal -= rectangleMoved _
    rectangle.changes -= rectangleMoved _
    rectangle.normal_old -= rectangleMoved2 _
  }

//def pointMoved() {
//  println("point moved")
//}

  def rectangleMoved(x: Int,y: Int) {
    println("rectangle moved by x: " + x + " y: " + y )
  }

  def rectangleMoved2(x: Int,y: Int) {
    println("rectangle2 moved by x: " + x + " y: " + y )
  }

//def rectangleResized() {
//  println("rectangle resized")
//}
}
