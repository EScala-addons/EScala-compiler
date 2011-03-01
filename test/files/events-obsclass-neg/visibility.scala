import scala.events.allInstances

private observable class Klass (val param: String) {

}

object Test {
    def main (args: Array[String]) {
        val k1 = new Klass("a")

    }
}

// vim: set ts=4 sw=4 et:
