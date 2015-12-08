import java.io.FileReader

/**
 * @author corajr
 */
object LightsApp extends App {
  type Lights = Array[Array[Int]]

  val reader = new FileReader("../input.txt")
  val parsed = InstructionsParser.parseAll(InstructionsParser.instructions, reader)
  val instructions = parsed match {
    case InstructionsParser.Success(result, _) => result 
    case failure : InstructionsParser.NoSuccess => scala.sys.error(failure.msg)
  }
  
  val lights = Array.fill(1000, 1000)(0)
  
  def modifyRegion(arr: Lights, l: (Int, Int), r: (Int, Int))(f: (Int => Int)): Unit = {
    for (
      i <- l._1 to r._1;
      j <- l._2 to r._2
    ) {
      arr(i)(j) = f(arr(i)(j))
    }
  }
  
  for (instruction <- instructions;
      t = instruction.t;
      l = instruction.l;
      r = instruction.r
  ) {
    t match {
      case TurnOn => modifyRegion(lights, l, r) { _ + 1 }
      case TurnOff => modifyRegion(lights, l, r) { x => if (x >= 1) x - 1 else 0 }
      case Toggle => modifyRegion(lights, l, r) { _ + 2 }
    }
  }
  
  println(lights.map(_.sum).sum)
}