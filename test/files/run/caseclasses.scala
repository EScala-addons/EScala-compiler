case class Foo(x: int)(y: int);

case class Bar;

object Test extends Application {
  def fn[a,b](x: a => b) = x;
  val f = fn(Foo(1))
  (f(2): AnyRef) match {
    case Foo(1) => System.out.println("OK")
    case Bar() => System.out.println("NO")
  }

    try {
      Bar() caseElement 2
      throw new NullPointerException("duh")
    } catch {
      case x:IndexOutOfBoundsException =>
    }
    
    try {
      f(2) caseElement 2
      throw new NullPointerException("duh")
    } catch {
      case x:IndexOutOfBoundsException =>
    }

}
  
