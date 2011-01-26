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

  def moveBy(dx: Int, dy: Int) = {
    x += dx
    y += dy
  }

  evt normal(x: Int, y: Int) = moved(x,y)
  evt normal_old(x: Int, y: Int) = moved.map((a: Int,b: Int)=>(a,b))(x,y)

  evt change_order(x: Int, y: Int) = moved(y,x)

  evt drop_first(y :Int) = moved(_,y)
  evt drop_second(x: Int) = moved(x,_)
//  evt drop_all() = moved(_,_)

  evt unbound_left(a: Int, y: Int) = moved(x,y)
  evt unbound_right(x: Int, y: Int) = moved(x, b)

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
    
    rectangle.normal += evtNormalMsg _
    rectangle.normal_old += evtNormalMsg _
    rectangle.change_order += evtChangeMsg _
    rectangle.drop_first += evtDropFirstMsg _
    rectangle.drop_second += evtDropSecondMsg _
//  rectangle.drop_all += evtDropAllMsg _
    rectangle moveBy(1,3)
    rectangle.normal -= evtNormalMsg _
    rectangle.normal_old -= evtNormalMsg _
    rectangle.change_order -= evtChangeMsg _
    rectangle.drop_first -= evtDropFirstMsg _
    rectangle.drop_second -= evtDropSecondMsg _
//    rectangle.drop_all -= evtDropAllMsg _
  }

//def pointMoved() {
//  println("point moved")
//}

  def evtNormalMsg(x: Int,y: Int) {
    println("original order: x: " + x + " y: " + y )
  }

  def evtChangeMsg(x: Int,y: Int) {
    println("changed order: by x: " + x + " y: " + y )
  }

  def evtDropFirstMsg(x: Int) {
    println(" First dropped, second left: y: " + x)
  }

  def evtDropSecondMsg(x: Int) {
    println(" Second dropped, first left: x: " + x)
  }

  def evtDropAllMsg() {
    println("Dropped all")
  }
//def rectangleResized() {
//  println("rectangle resized")
//}
}
