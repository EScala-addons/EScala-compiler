class help
{
	observable var field = "testField"
}

class sub extends help
{
	evt testHelp = afterSet(field)
}