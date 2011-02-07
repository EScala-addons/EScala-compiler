import scala.events._

class C {
  imperative evt e1[Int, Int]

  evt e2(a: Int, b: Int) = e1(a+b, b)
}
