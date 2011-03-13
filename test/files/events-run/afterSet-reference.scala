class help
{
	observable var setTesterParent : String = "Test Observable"
	var setTesterImplicitParent = "Test Implicit"

}

class sub extends help
{
	observable var setTesterThis = "var def in this"
	evt afterSetThis = afterSet(setTesterThis)
	evt beforeSetThis = beforeSet(setTesterThis)
	
	evt afterSetParentObservable = afterSet(setTesterParent)
	evt beforeSetParentImplicit = beforeSet(setTesterImplicitParent)
}

object Test {
  def main(args: Array[String]) {
    val s = new sub
    
    s.beforeSetParentImplicit += reactBefore _
    s.afterSetParentObservable += reactAfter _
    
    s setTesterParent = "Test Observable var update"
    s setTesterImplicitParent = "Test Implicit var update"
    
    def reactBefore(oldVal : String, newVal : String) = println("BeforeEvent: replacing -" + oldVal + "-\nwith -" + newVal + "-")
  	def reactAfter(newVal : String) = println("After-Event, assigned value:" + newVal)
  }

}