import scala.events._

class Rectangle(){
  var x = 0;
  var y = 0;

  evt moved[(Int,Int)] = afterExec(moveBy).map((_:Any) => (1,2))

  def moveBy(dx: Int, dy: Int) = {
    x += dx
    y += dy
  }

  evt drop_first(y :Int) = moved(_,y)
  evt drop_second(x: Int) = moved(x,_)
  evt drop_all() = moved(_,_)
}

object Test {
  def main(args: Array[String]) {
    val rectangle = new Rectangle()
    rectangle.drop_first += evtDropFirstMsg _
    rectangle.drop_second += evtDropSecondMsg _
    rectangle.drop_all += evtDropAllMsg _
    rectangle moveBy(1,3)
    rectangle.drop_first -= evtDropFirstMsg _
    rectangle.drop_second -= evtDropSecondMsg _
    rectangle.drop_all -= evtDropAllMsg _
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
}
