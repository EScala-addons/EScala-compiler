import scala.events.anyInstance

observable class Transaction {

    observable def credit() {
        System.out.println("Credit")
    }

    observable def debit() {
        System.out.println("Debit")
    }
}

class TransactionManager {

    evt e_bcredit[Unit] = beforeExec(anyInstance[Transaction].credit)
    
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
        //System.out.println("Objects : "+anyInstance[Transaction])
    }
}

