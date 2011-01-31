import scala.events._

class C{
  imperative evt e1[Int,Int]

  evt e2(x: Int) = e1(x)
  evt e3(x: Int, y: Int, z: Int) = e1(x,y,z)
}
