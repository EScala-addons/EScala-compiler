class help
{
	observable var field = "testField"
	
}

class sub extends help
{
	evt testHelp = afterSet(field)
}
	

object Test
{
	def main(args: Array[String]) {
		val subTester = new sub()

	}
	
}