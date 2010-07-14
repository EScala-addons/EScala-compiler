class C {
  imperative evt e[Int]

  e += react _

  def react(i: Int) {
    println("received " + i)
  }
}

object Test {
  def main(args: Array[String]) {
    val c = new C
    c.e(1)
    c.e(4)
  }
}
// vim: set ts=4 sw=4 et:
