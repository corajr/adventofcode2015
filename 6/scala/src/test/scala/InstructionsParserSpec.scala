import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import java.io.FileReader

class InstructionsParserSpec extends FunSpec with ShouldMatchers {
  
  it("should parse the input file") {
	  val reader = new FileReader("../input.txt")
    val parsed = InstructionsParser.parseAll(InstructionsParser.instructions, reader)
    parsed match {
      case InstructionsParser.Success(result, _) => result.size == 300
      case failure : InstructionsParser.NoSuccess => scala.sys.error(failure.msg)
    }
  }
}
