import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import java.io.FileReader

class InstructionsParserSpec extends FunSpec with ShouldMatchers {
  
  it("should parse the input file") {
	  val reader = new FileReader("../input.txt")
    val parsed = InstructionsParser.parseAll(InstructionsParser.instructions, reader)
    val result = parsed match {
      case InstructionsParser.Success(result, _) => result
      case failure : InstructionsParser.NoSuccess => scala.sys.error(failure.msg)
    }
    
    result.size shouldBe 300
    result(0) shouldBe Instruction(TurnOn, (887,9), (959,629))
    result(2) shouldBe Instruction(TurnOff, (539,243), (559,965))
    result(7) shouldBe Instruction(Toggle, (720,196), (897,994))
  }
}
