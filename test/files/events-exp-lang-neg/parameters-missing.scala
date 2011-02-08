import scala.events._

class C {
  imperative evt e1[Int]

  evt e2(x: Int) = e1

  evt e3(x: Int) = e1(_)
}
