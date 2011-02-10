import scala.events.anyInstance

observable class Father {
    observable def credit() {println("Credit")}
}

observable class Transaction extends Father

object Test {

    def beforeCredit() {println("Be prepared to receive money")}

    def main(args: Array[String]) {
        evt e_bcredit[Unit] = beforeExec(anyInstance[Transaction].credit)

        e_bcredit += beforeCredit _

        var t1 = new Transaction
        t1.credit
    }
}
