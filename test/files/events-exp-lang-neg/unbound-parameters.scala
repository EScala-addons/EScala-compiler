import scala.events._

class C {
  imperative evt e1[Int,Int]

  evt e2(x: Int, y: Int) = e1(a,y)
  evt e3(x: Int, y: Int) = e1(x,b)
}
