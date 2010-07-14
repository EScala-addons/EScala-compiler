trait C1 {
  evt e1[Int]
  evt e2[Int]
  evt e3[Int] = e1 || e2
  evt e4[Int, Int] = e3.map((x : Int) => (x, x))
}

class C2 extends C1 {
  imperative evt e1[Int]
  evt e2 = e1  
}

class C3(val o1 : C1) {
  evt e1 = o1.e4
  
  e1 += react1 _
  (o1.e2 || o1.e3) += react2 _
  
  def react1(x: Int, y: Int) { println("react1") }
  def react2(x: Int) { println("react2") }
}

object Test {
  def main(args: Array[String]) {
    val o1 = new C2
    val o2 = new C3(o1)
    o1.e1(2)
    o1.e1(3)    
  }
}
