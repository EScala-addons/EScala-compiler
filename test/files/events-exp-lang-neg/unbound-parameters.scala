import scala.events._

class Test(){
  imperative evt event[Int,Int]

  evt unbound_left(x: Int, y: Int) = moved(a,y)
  evt unbound_right(x: Int, y: Int) = moved(x, b)
}
