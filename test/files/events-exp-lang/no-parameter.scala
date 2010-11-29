import scala.events._

class Point(var x: Int, var y: Int){    
  evt moved() = afterExec(moveBy)

  def moveBy(dx: Int, dy: Int) = {
    x += dx
    y += dy
  }
}

class Rectangle(val upperleft: Point, val lowerright: Point){
  evt resized() = lowerright.moved()
  evt moved() = upperleft.moved()

  def resizeBy(dx: Int, dy: Int) = {
    lowerright.moveBy(dx, dy)
  }
}

object Test {
  def main(args: Array[String]) {
    val point = new Point(5, 5)
    val rectangle = new Rectangle(new Point(3, 3), new Point(5, 5))

    point.moved += pointMoved _
    point.moveBy(2, 2)
    point.moved -= pointMoved _

    rectangle.resized += rectangleResized _
    rectangle resizeBy(1, 2)
    rectangle.resized -= rectangleResized _
  }

  def pointMoved() {
    println("point moved")
  }

  def rectangleResized() {
    println("rectangle resized")
  }
}
