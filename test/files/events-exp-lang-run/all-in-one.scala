import scala.events._

class A {

}

class B extends A {

}

abstract class D {
  imperative evt e0[A, B]

  evt e2() = m()(_,_)

  def m() = {e0}

}

class C extends D {
  imperative evt e1[Int, Int]

//  override evt e2(a:Int , b: Int) = e1(a+b,b)

//  evt e3(s2: String, x: Int) = e2(_,s2,y,x)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    val o2: C = o
    val a = new A
    val b = new B

    o2.e2 += { () => println("e2") }

    //o.e1(a, b)
    o.e0(a, b)

    //o.e2 += { (s1: String, s2: String, x: Int,y: Int) => println("e2 "+s1+" "+s2+" "+x+" "+y) }
    //o.e3 += { (s1: String, x: Int) => println("e3 "+s1+" "+x) }
    //o.e1("Hello", true, 2, "World", false, 4)
  }
}
