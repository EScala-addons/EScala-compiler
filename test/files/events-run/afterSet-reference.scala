class help
{
	observable var setTesterParent : String = "var def in parent"
	var setTesterImplicitParent = "var !observable and def in parent"
	
	//observable def execTesterObservableParent(tempVar : String) = println("tempVar: " + tempVar)
	//def execTesterParent(counter : Int) = println("param value: " + counter.toString)
}

class sub extends help
{
	//observable var setTesterThis = "var def in this"
	//evt testSetObservableThis = afterSet(setTesterThis)
	
	//evt testSetObservableParent = afterSet(setTesterParent)
	//evt testSetImplicitParent = afterSet(setTesterImplicitParent)
	
	//evt testExecObservableParent = afterExec(execTesterObservableParent)
	//evt testExecParent = afterExec(execTesterParent)
	
	// var unused = "unusedVar"
	//def reactBefore(i: Int) = println("react before m(" + i + ")")
	
	
	//var implicitDef = "implicitTest" 
	//evt testImplicit = afterSet(implicitDef)
}

object Test {
  def main(args: Array[String]) {
    val s = new sub
    evt testSetObservableParent = afterSet(s.setTesterParent)
    testSetObservableParent += reactAfter _
    s setTesterParent = "updated var def"
  }

  //def reactBefore(i: Int) = println("react before m(" + i + ")")

  def reactAfter(str : String) = println("AfterEvent handled with value: " + str)

}