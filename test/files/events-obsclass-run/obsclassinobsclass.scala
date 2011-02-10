import scala.events.allInstances

class TransactionManager {

    observable class Transaction {
        
        observable def credit() {
            val l = new LogTransaction
            l.write("Credit")
        }

        observable def debit() {
            val l = new LogTransaction
            l.write("Debit")
        }

        observable class LogTransaction {
            
            observable def write(s: String) {
                System.out.println("Log write : " +s)
            }
        }

       evt e_blog[Unit] = beforeExec(allInstances[LogTransaction].write)
       e_blog += beforeLog _
       evt e_alog[Unit] = afterExec(allInstances[LogTransaction].write)
       e_alog += afterLog _
       
       def beforeLog() {
           System.out.println("Opening log")
        }

       def afterLog() {
           System.out.println("Closing log")
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

