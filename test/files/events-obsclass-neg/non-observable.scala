import scala.events.allInstances

class Klass

object Test {
    def main(args: Array[String]) {
        val all = allInstances[Klass]
    }
}

