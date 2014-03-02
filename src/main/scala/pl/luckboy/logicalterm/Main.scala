package pl.luckboy.logicalterm
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._
import scala.tools.jline.console.ConsoleReader

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
  def readStringLoop(s: String): String = {
    val line = consoleReader.readLine()
    if(line =/= null) readStringLoop(s + line + "\n") else s
  }
  
  def readString() = {
    val savedPrompt = consoleReader.getPrompt()
    consoleReader.setPrompt("")
    val s = readStringLoop("")
    consoleReader.setPrompt(savedPrompt)
    s
  }
  
  @tailrec
  def mainLoop[T[_]](exec: Executor { type Table[U] = T[U] })(table: T[Int]): Unit = {
    consoleReader.setPrompt("logicalterm> ")
    val line = consoleReader.readLine()
    if(line =/= null) {
      val (table3, exitFlag) = parseCommand(line).map {
        pair =>
           pair match {
            case ("help", _)   =>
              consoleReader.println("Commands:")
              consoleReader.println(":help           display this text")
              consoleReader.println(":paste          enter the paste mode (exit from this mode is Ctrl-D)")
              consoleReader.println(":quit           exit this program")
              consoleReader.println(":term <term>    display the term")
              (table, ExitFlag.NoExit)
            case ("paste", _)  =>
              val s = readString()
              (Parser.parseString(s).flatMap {
                instrs =>
                  withTime { exec.execute(instrs)(table) }.map {
                    case (table2, reses) =>
                      for(res <- reses) { consoleReader.println(res.toString) }
                      table2
                  }
              }.valueOr { 
                err =>
                  consoleReader.println(err.toString)
                  table
              }, ExitFlag.NoExit)
            case ("quit", _)   =>
              (table, ExitFlag.Exit)
            case ("term", arg) =>
              Parser.parseTermString(arg).map {
                term =>
                  val matchingTerm = withTime { exec.matchingTermFromTerm(term) }
                  consoleReader.println(matchingTerm.toString)
              }.valueOr { err => consoleReader.println(err.toString) }
              (table, ExitFlag.NoExit)
            case _             =>
              consoleReader.println("unknown command " + line)
              (table, ExitFlag.NoExit)
          }
      }.getOrElse {
        (Parser.parseInstructionString(line).flatMap {
          instr =>
            withTime { exec.executeInstruction(instr)(table) }.map {
              case (table2, res) =>
                consoleReader.println(res.toString)
                table2
            }
        }.valueOr { 
          err =>
            consoleReader.println(err.toString)
            table
        }, ExitFlag.NoExit)
      }
      exitFlag match {
        case ExitFlag.Exit   => ()
        case ExitFlag.NoExit => mainLoop[T](exec)(table3)
      }
    }
  }
  
  val executors = Map(
      "simple" -> simpleExecutor)
  
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
