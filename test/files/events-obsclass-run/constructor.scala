import scala.events.anyInstance

observable class Transaction (val param: String) {

    def this(param_other_1: String, param_other_2: String) {
        this(param_other_1 + param_other_2)
    }

    observable def credit() {println("Credit")}
}


object Test {

    def beforeCredit() {println("Be prepared to receive money")}

    def main(args: Array[String]) {
        evt e_bcredit[Unit] = beforeExec(anyInstance[Transaction].credit)

        e_bcredit += beforeCredit _

        var t1 = new Transaction("test", " string")
        println(t1.param)
        t1.credit
    }
}


