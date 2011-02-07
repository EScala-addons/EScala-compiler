import scala.events._

class A {
}
class B extends A {
}

class C {
  imperative evt e1[A, B]

  evt e2(a: A, b: B) = e1(a,b)

  evt e3(a1: A, a2: A) = e1(a1,a2)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    val a = new A
    val b = new B
    val b2 = new B

    o.e3 += { (a1: A, a2: A) => println("e3") }
    o.e2 += { (b1: B, b2: B) => println("e22") }

    o.e1(a, b)
    o.e1(b2, b)
  }
}
