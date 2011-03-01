import scala.events.allInstances

observable class Klass[T] (val param: T) {
}

object Test {
    def main (args: Array[String]) {
        val k1 = new Klass[String]("a")
        val k2 = new Klass[String]("b")
        val k3 = new Klass[String]("c")
        val k4 = new Klass[Int](4)
        val k5 = new Klass[Int](5)

        // Parametized type will be ignored, so we can put null for example
        allInstances[Klass[Null]].foreach(k => println("Klass('" + k.param + "')"))
    }
}

// vim: set ts=4 sw=4 et:
