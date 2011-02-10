import scala.events.allInstances

observable class Klass (val param: String) {
}

object Test {
    def main (args: Array[String]) {
        val k1 = new Klass("a")
        val k2 = new Klass("b")
        val k3 = new Klass("c")

        allInstances[Klass].foreach(k => println("Klass('" + k.param + "')"))
    }
}
