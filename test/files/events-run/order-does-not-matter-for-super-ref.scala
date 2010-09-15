class A extends B {
  override def m(i: Int) = super.m(i)
}

class B {
  evt before[Unit] = beforeExec(m)
  evt after[Unit] = afterExec(m)
  def m(i: Int) = println(i)
  before += (() => println("before exec"))
  after += (() => println("after exec"))
}

object Test {
  def main(args: Array[String]) {
    val a = new A
    a.m(3)
  } 
}

// vim: set ts=4 sw=4 et:
