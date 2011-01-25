class help
{
	var field = "testField" 
	var noeventfield = "notUsed" 
}

class sub extends help
{
	evt testHelp = afterSet(field)
}