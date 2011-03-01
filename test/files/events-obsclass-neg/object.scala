import scala.events.anyInstance

observable class Father {
    observable def credit() {println("Credit")}
}
observable object Transaction extends Father

object Test {

    def beforeCredit() {println("Be prepared to receive money")}

    def main(args: Array[String]) {
        evt e_bcredit[Unit] = beforeExec(anyInstance[Transaction].credit)

        e_bcredit += beforeCredit _

        Transaction.credit
    }
}



