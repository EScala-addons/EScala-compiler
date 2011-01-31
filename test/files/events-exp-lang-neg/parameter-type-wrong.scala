import scala.events._

class C{
  imperative evt e1[String]
  evt e2(x: Int) = e1(x)

  imperative evt e3[Int,String]
  evt e4(x: Int,y: String) = e3(y,x)
}
