class help
{
	observable var setTesterParent = "var def in parent"
	var setTesterImplicitParent = "var !observable and def in parent"
	
	//observable def execTesterObservableParent(tempVar : String) = println("tempVar: " + tempVar)
	//def execTesterParent(counter : Int) = println("param value: " + counter.toString)
}

class sub extends help
{
	//observable var setTesterThis = "var def in this"
	//evt testSetObservableThis = afterSet(setTesterThis)
	
	evt testSetObservableParent = afterSet(setTesterParent)
	evt testSetImplicitThis = afterSet(setTesterImplicitParent)
	
	//evt testExecObservableParent = afterExec(execTesterObservableParent)
	//evt testExecParent = afterExec(execTesterParent)
	
	// var unused = "unusedVar"
	
	//var implicitDef = "implicitTest" 
	//evt testImplicit = afterSet(implicitDef)
}