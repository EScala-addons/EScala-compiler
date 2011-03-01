import scala.events.anyInstance

trait Trait {
    observable def credit() {println("Credit")}
}

observable class Transaction extends Trait

object Test {

    def beforeCredit() {println("Be prepared to receive money")}

    def main(args: Array[String]) {
        evt e_bcredit[Unit] = beforeExec(anyInstance[Transaction].credit)

        e_bcredit += beforeCredit _

        var t1 = new Transaction
        t1.credit
    }
}


