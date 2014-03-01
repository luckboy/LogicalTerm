package pl.luckboy.logicalterm
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._
import scala.tools.jline.console.ConsoleReader
import pl.luckboy.logicalterm.simple.termMatcher

object Main
{
  object ExitFlag extends Enumeration
  {
    val Exit, NoExit = Value
  }
  
  val consoleReader = new ConsoleReader()
  
  def withTime[T](f: => T) = {
    val startTime = System.currentTimeMillis
    val y = f
    consoleReader.println("Time: " + ((System.currentTimeMillis - startTime) / 1000.0) + "s")
    y
  }
  
  def parseCommand(line: String) =
    line.split("\\s+", 2).toList match {
      case s :: ss if s.startsWith(":") => some((s.tail, ss.headOption.getOrElse("")))
      case _                            => none
    }
  
  @tailrec
  def mainLoop[T[_]](exec: Executor { type Table[U] = T[U] })(table: T[Int]): Unit = {
    consoleReader.setPrompt("logicalterm> ")
    val line = consoleReader.readLine()
    if(line =/= null) {
      val (table4, exitFlag) = parseCommand(line).map {
        pair =>
          (table, pair match {
            case ("help", _)   =>
              consoleReader.println("Commands:")
              consoleReader.println(":help           display this text")
              consoleReader.println(":term <term>    display the term")
              consoleReader.println(":quit           exit this program")
              ExitFlag.NoExit
            case ("term", arg) =>
              Parser.parseTermString(arg).map {
                term =>
                  val matchingTerm = withTime { exec.matchingTermFromTerm(term) }
                  consoleReader.println(matchingTerm.toString)
              }.valueOr { err => consoleReader.println(err.toString) }
              ExitFlag.NoExit
            case ("quit", _)   =>
              ExitFlag.Exit
          })
      }.getOrElse {
        Parser.parseInstructionString(line).map {
          instr =>
            val table3 = withTime { exec.executeInstruction(instr)(table) }.map {
              case (table2, res) =>
                consoleReader.println(res.toString)
                table2
            }.valueOr {
              err =>
                consoleReader.println(err.toString)
                table
            }
            (table3, ExitFlag.NoExit)
        }.valueOr { 
          err =>
            consoleReader.println(err.toString)
            (table, ExitFlag.NoExit)
        }
      }
      exitFlag match {
        case ExitFlag.Exit   => ()
        case ExitFlag.NoExit => mainLoop[T](exec)(table4)
      }
    }
  }
  
  val executors = Map(
      "simple" -> Executor.executor[Term, simple.TableP[Term]#A])
  
  def main(args: Array[String]): Unit = {
    val execName = args.headOption.getOrElse("simple")
    executors.get(execName).map {
      executor =>
        mainLoop[executor.Table](executor)(executor.emptyTable)
    }.getOrElse {
      Console.err.println("unknown executor")
    }
  }
}
