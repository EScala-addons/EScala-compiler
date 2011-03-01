import scala.events.anyInstance
import scala.events.allInstances


observable class Transaction {
    observable def credit() {}
}

class TransactionManager {
    // Redefinition of anyInstance
    def anyInstance: String = "foo"

    evt e_bcredit[Unit] = beforeExec(anyInstance[Transaction].credit)
}

object Test {
    def allInstances: String = "bar"

    def main(args: Array[String]) {
        var t = new TransactionManager
        var l = allInstances[Transaction]

        var t1 = new Transaction
        t1.credit
        
    }
}


