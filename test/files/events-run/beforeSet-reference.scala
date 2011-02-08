class help
{
	observable var setObservableParent = "var in parent"
}

class sub extends help
{
	//observable var setObservableThis = "var in this"
	
	evt testObservableParent = beforeSet(setObservableParent)
	//evt testObservableThis = beforeSet(setObservableThis)
}