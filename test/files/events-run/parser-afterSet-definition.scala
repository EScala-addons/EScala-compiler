import scala.events._
object Test
{
	var field1 = "test"
	
	evt testAfter = afterSet(field1)
	
}