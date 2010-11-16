import scala.events.VarList

class Transaction {

    observable def credit() {
        System.out.println("Credit")
    }

    observable def debit() {
        System.out.println("Debit")
    }
}

class TransactionManager {

//    evt e_bcredit = BeforeExec(Transaction.credit)
    //evt e_bcredit = TransactionList.all.any(e => beforeExec(e.credit)) => Marche pô :(
    evt e_bcredit[Unit] = TransactionList.all.any(e => beforeExec(e.credit))
    
    e_bcredit += BeforeCredit

    def BeforeCredit() {
        System.out.println("Be prepared to receive money")
    }
}

object TransactionList {
    var all = new VarList[Transaction]
}

object Test {
    def main(args: Array[String]) {
        var t = new TransactionManager

        var t1 = new Transaction
        TransactionList.all += t1
        var t2 = new Transaction
        TransactionList.all += t2
        t1.credit
        t2.debit
    }
}


// vim: set ts=4 sw=4 et:
