abstract class C1 {    
    observable def m(x: Int, y : Int) { println("C1.m") }
    observable def o(x: Int) 
    observable def p(x: Int) 
    
    beforeExec(m) += ((x : Int, y : Int) => { println("before m " + x + ":" + y) })
    beforeExec(o) += ((x : Int) => { println("before o " + x) })
    beforeExec(p) += ((x : Int) => { println("before p " + x) })     
}

abstract class C2 extends C1 {
     observable def n(x: Int) { println("C2.n") }
     
     beforeExec(n) += ((x : Int) => { println("before n " + x) })
     
     observable def o(x: Int) { println("C2.o " + x) }
} 

class C3 extends C2 {
    override observable def m(x: Int, y : Int) { println("C3.m " + x + ":" + y) }
    override observable def n(x: Int) { println("C3.n " + x) }
    observable def p(x: Int) { println("C3.p " + x) }
}

object Test {
  def main(args: Array[String]) {
    val c3 = new C3
    c3.m(1, 1)
    c3.n(2)
    c3.o(3)
    c3.p(4)    
  }
}
