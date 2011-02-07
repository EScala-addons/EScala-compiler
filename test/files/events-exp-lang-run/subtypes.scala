import scala.events._

class A {
}
class B extends A {
}

class C {
  imperative evt e1[A, B]

  evt e2(a: A, b: B) = e1(a,b)

  evt e3(a1: A, a2: A) = a1(a1,a2)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    val o2: C = o
    val a = new A
    val b = new B

    o.e2 += { (a: A, b: B) => println("e2") }
    o.e3 += { (a1: A, a2: A) => println("e3") }

    o.e1(a, b)
  }
}
