class help
{
	observable var field = "testField"
	//observable val valuefield = "testValue"
	
}

class sub extends help
{
	evt testHelp = afterSet(field)
}

class alone
{
		observable var dummy = "test"
		evt testingDummy = afterSet(dummy)
}
	

object Test
{
	def main(args: Array[String]) {
		//val subTester = new sub()
		val subTester = new alone()
	}
	
}