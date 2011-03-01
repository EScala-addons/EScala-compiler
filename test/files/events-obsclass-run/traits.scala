import scala.events.anyInstance

observable trait Trait {
    observable def credit() {println("Credit")}
}

class Transaction extends Trait

object Test {

    def beforeCredit() {println("Be prepared to receive money")}

    def main(args: Array[String]) {
        evt e_bcredit[Unit] = beforeExec(anyInstance[Trait].credit)

        e_bcredit += beforeCredit _

        var t1 = new Transaction
        t1.credit
    }
}


