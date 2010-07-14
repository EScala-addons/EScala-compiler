class D extends A {
  // instrument m in this class
  evt e[Unit] = beforeExec(m)
}

class A {
  def m() { println("m") }
}

class E extends C {
  // do not instrument m in this class because already instrumented in B
  evt e2[Unit] = afterExec(m)
}

class C extends B 

class B extends A {
  // instrument m in this class
  evt e[Unit] = beforeExec(m)
}

object Test {
  def main(args: Array[String]) {
    val a = new A
    val b = new B
    val c = new C
    val d = new D
    val e = new E
    b.e += {() => println("before m in B")}
    c.e += {() => println("before m in C")}
    d.e += {() => println("before m in D")}
    e.e += {() => println("before m in E")}
    e.e2 += {() => println("after m in E")}

    a.m()
    b.m()
    c.m()
    d.m()
    e.m()
  }
}

// vim: set ts=4 sw=4 et:
