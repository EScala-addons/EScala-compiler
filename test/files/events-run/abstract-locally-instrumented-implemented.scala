class B extends A {
  def m(i: Int) = i.toString
}

abstract class A {

  evt e = beforeExec(m)

  def m(i: Int): String
}

object Test {
  def main(args: Array[String]) {
    val b = new B

    b.e += {i => println("before m(" + i + ")")}

    println(b.m(2))
  }
}

// vim: set ts=2 sw=2 et:
