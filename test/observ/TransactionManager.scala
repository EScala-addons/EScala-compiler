package truie.toto

import truie.pipo.Transaction
import scala.events.VarList
import scala.events.allInstances

class TransactionManager {

    evt e_bcredit[Unit] = beforeExec(allInstances[Transaction].credit)
    
    e_bcredit += beforeCredit _

    def beforeCredit() {
        System.out.println("Be prepared to receive money")
    }
}

object Test {
    def main(args: Array[String]) {
        var t = new TransactionManager

        var t1 = new Transaction
        var t2 = new Transaction
        t1.credit
        t2.credit
        t2.debit
    }
}

