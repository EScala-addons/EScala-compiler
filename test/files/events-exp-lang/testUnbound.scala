import scala.events._

class Rectangle(){
  var x = 0;
  var y = 0;

  evt moved[(Int,Int)] = afterExec(moveBy).map((_:Any) => (1,2))

  def moveBy(dx: Int, dy: Int) = {
    x += dx
    y += dy
  }

  evt unbound_left(a: Int, y: Int) = moved(x,y)
  evt unbound_right(x: Int, y: Int) = moved(x, b)
}

object Test {
  def main(args: Array[String]) {
    val rectangle = new Rectangle()
  }
}
