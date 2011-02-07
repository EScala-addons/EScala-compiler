import scala.events._

class A {
}
class B extends A {
}

class C {
  imperative evt e1[B]
  imperative evt e2[A, B]

  evt e3(b: B) = e1(b)
  evt e4(a: A, b: B) = e2(a, b)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    val a1 = new A
    val a2 = new A

    o.e1(a1)
    o.e2(a1, a2)
  }
}
