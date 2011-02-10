import scala.events.allInstances

class TransactionManager {

    observable class Transaction {

        observable def credit() {
            System.out.println("Credit")
        }

        observable def debit() {
            System.out.println("Debit")
        }
    }

    evt e_bcredit[Unit] = beforeExec(allInstances[Transaction].credit)
    
    e_bcredit += beforeCredit _

    def beforeCredit() {
        System.out.println("Be prepared to receive money")
    }

    def run() {
        var t1 = new Transaction
        var t2 = new Transaction
        t1.credit
        t2.credit
        t2.debit
    }
}

object Test {
    def main(args: Array[String]) {
        var t = new TransactionManager
        t.run()
    }
}

