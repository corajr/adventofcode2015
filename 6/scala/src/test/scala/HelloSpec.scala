import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class HelloSpec extends FunSpec with ShouldMatchers {
  
  it("should pass the simplest of tests") {
	  "hello" should be ("hello")
  }
}
