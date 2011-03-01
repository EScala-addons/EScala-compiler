import scala.events.anyInstance

observable class Transaction {
    observable def credit(x: Int) {println("Credit")}
}

object Test {

    def beforeCredit(x: Int) {println("Be prepared to receive money")}

    def main(args: Array[String]) {
        evt e_bcredit = beforeExec(anyInstance[Transaction].credit).map((x: Int) => x + x)

        e_bcredit += beforeCredit _

        val t1 = new Transaction
        t1.credit(6)
    }
}



