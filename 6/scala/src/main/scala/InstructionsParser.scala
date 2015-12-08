import scala.util.parsing.combinator.RegexParsers

object InstructionsParser extends RegexParsers {
  override type Elem = Char
  def integer     = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def region      = repsep(integer, ",") ^^ { case Seq(x,y) => (x,y) }
  def task        = "turn on" ^^ (_ => TurnOn) |
                    "turn off" ^^ (_ => TurnOn) |
                    "toggle" ^^ (_ => Toggle)
  def instructions = instruction*
  def instruction : Parser[Instruction] = task~region~"through"~region ^^
    { case task~region~t~region2 => Instruction(task, region, region2) }

}

sealed trait Task
case object TurnOn extends Task
case object TurnOff extends Task
case object Toggle extends Task

case class Instruction(t: Task, l: (Int,Int), r: (Int, Int))