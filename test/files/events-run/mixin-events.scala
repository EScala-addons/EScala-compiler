trait C1 {
  protected[this] imperative evt impA[Int]
  evt eA[Int] = impA

  eA += reactC1A _

  def doSomething() { 
     impA(1) 
  }
  
  def reactC1A(x : Int) = println("reactC1A")
}
trait C2 extends C1 {
  protected[this] imperative evt impB[Int]
  override evt eA[Int] = super.eA || impB
  
  override def doSomething() {
     super.doSomething()      
     impB(2)
  }
    
}
trait C3 extends C1 {
  protected[this] imperative evt impC[Int]
  override evt eA[Int] = super.eA || impC
  
  override def doSomething() {
     super.doSomething()      
     impC(3)
  } 
}

class C4 extends C2 with C3 {

}


object Test {
  def main(args: Array[String]) {
    val c1 = new C4    
    c1.doSomething()
  }
}

// vim: set ts=4 sw=4 et:
