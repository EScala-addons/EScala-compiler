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

  evt normal(x: Int, y: Int) = moved(x,y)
  evt normal_old(x: Int, y: Int) = moved.map((a: Int,b: Int)=>(a,b))(x,y)

}

object Test {
  def main(args: Array[String]) {
    val rectangle = new Rectangle()

    rectangle.normal += evtNormalMsg _
    rectangle.normal_old += evtNormalMsg _
    rectangle moveBy(1,3)
    rectangle.normal -= evtNormalMsg _
    rectangle.normal_old -= evtNormalMsg _
  }

  def evtNormalMsg(x: Int,y: Int) {
    println("original order: x: " + x + " y: " + y )
  }
}
