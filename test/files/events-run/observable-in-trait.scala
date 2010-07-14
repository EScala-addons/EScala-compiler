trait C1 {    
    observable def m(x: Int) { println("C1.m") }    
    beforeExec(m) += ((_ : Int) => { println("before m") })        
}

class C2 extends C1 {
    override observable def m(x: Int) { println("C2.m") }     
} 

object Test {
  def main(args: Array[String]) {
    val o = new C2
    o.m(1)        
  }
}
