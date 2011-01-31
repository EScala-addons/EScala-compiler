import scala.events._

class C{
  imperative evt e1[String]

  evt e2(x: Int) = e1(x)
}
