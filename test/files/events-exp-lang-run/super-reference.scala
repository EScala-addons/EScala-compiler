class C1 {
  imperative evt e1[Unit]
}

class C2 extends C1 {
  evt e2() = super.e1()
}

object Test {
  def main(args: Array[String]) {
    val o = new C2
    o.e2 += { () => println("e2") }
    o.e1()
  }
}
