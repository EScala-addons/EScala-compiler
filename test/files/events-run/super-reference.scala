class C1 {
  private imperative evt impA[Unit]
  evt eA[Unit] = impA

  eA += reactC1A _

  def doSomething() {
    impA()
  }
  def reactC1A = println("reactC1A")
}
class C2 extends C1 {
  imperative evt impB[Unit]
  override evt eA[Unit] = super.eA || impB
  evt eB[Unit] = super.eA
  evt eC[Unit] = super.eA

  eA += reactC2A _
  eB += reactC2B _
  eC += reactC2C _

  def reactC2A = println("reactC2A")
  def reactC2B = println("reactC2B")
  def reactC2C = println("reactC2C")
}
object Test {
  def main(args: Array[String]) {
    val c1 = new C1
    val c2 = new C2
    c1.doSomething()
    c2.impB()
    c2.doSomething()
  }
}

// vim: set ts=4 sw=4 et:
