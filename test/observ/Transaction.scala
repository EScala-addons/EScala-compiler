package truie

import scala.events.VarList

observable class Transaction {

    observable def credit() {
        System.out.println("Credit")
    }

    observable def debit() {
        System.out.println("Debit")
    }
}

class TransactionManager {

//    evt e_bcredit = BeforeExec(Transaction.credit)
    evt e_bcredit = Transaction$all.all.any(e => beforeExec(e.credit)) 
    
    e_bcredit += BeforeCredit _

    def BeforeCredit() {
        System.out.println("Be prepared to receive money")
    }
}

//object Transaction {
//    var all = new VarList[Transaction]
//}

object Test {
    def main(args: Array[String]) {
        var t = new TransactionManager

        var t1 = new Transaction
        var t2 = new Transaction
        t1.credit
        t2.debit
        System.out.println("Objects : "+Transaction$all.all)
    }
}


// vim: set ts=4 sw=4 et:
