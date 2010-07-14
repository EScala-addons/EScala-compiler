import scala.collection.mutable.ListBuffer

class C1 {
    def m1(x: Int) { }
}

class C2 {
    var list = new ListBuffer[C1]
    
    observable def m2(x: Int) {
       for (item <- list) {
         item.m1(x)
       }
    }   
}

object Test {
  def main(args: Array[String]) {
    val c = new C2
    c.m2(2)
    println("foo")
  }
}
