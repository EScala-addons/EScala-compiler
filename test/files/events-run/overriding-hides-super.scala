class C1 {
  private imperative evt intern[Unit]
  evt e[Unit] = intern

  e += reactC1 _

  def doSomething() {
    intern()
  }
  def reactC1 = println("reactC1")
}
class C2 extends C1 {
  imperative evt imp[Unit]
  override evt e[Unit] = imp

  e += reactC2 _

  def reactC2 = println("reactC2")
}
object Test {
  def main(args: Array[String]) {
    val c1 = new C1
    val c2 = new C2
    c1.doSomething()
    c2.imp()
    c2.doSomething()
  }
}

// vim: set ts=4 sw=4 et:
