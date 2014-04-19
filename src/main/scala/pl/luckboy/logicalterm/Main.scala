package pl.luckboy.logicalterm
import java.io.Closeable
import java.io.File
import java.io.FileWriter
import java.io.PrintWriter
import java.io.IOException
import java.util.Date
import java.text.SimpleDateFormat
import scala.annotation.tailrec
import scala.io.Source
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
  
  def executeWithTime[T[_]](exec: Executor { type Table[U] = T[U] }, s: String)(table: T[Int]): T[Int] =
    Parser.parseString(s).flatMap {
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
    }
  
  def withClose(pw: Closeable)(f: => Unit): Unit = {
    try {
      f
    } finally {
      pw.close()
    }
  }
  
  def benchmarkPrintln(pw: PrintWriter, s: String) = {
    consoleReader.println(s)
    pw.println(s)
    pw.flush()
  } 
  
  def benchmark[T[_]](exec: Executor { type Table[U] = T[U] }, name: String, iters: Int, num: Int, execName: String) = {
    val sdf = new SimpleDateFormat("dd-MM-yyyy_hh-mm-ss")
    val inDirName = "benchmarks"
    val inFileName = inDirName + "/" + name + ".lt"
    val outDirName = "results"
    val outDirName2 = outDirName + "/" + name
    val outDirName3 = outDirName2 + "/" + execName
    val outFileName = outDirName3 + "/" + iters + "-" + num + "_" + sdf.format(new Date()) + ".txt"
    try {
      val s = Source.fromFile(inFileName).mkString("")
      new File(outDirName).mkdir
      new File(outDirName2).mkdir
      new File(outDirName3).mkdir
      val pw = new PrintWriter(new FileWriter(outFileName))
      withClose(pw) {
        for(i <- 0 until num) {
          Parser.parseString(s).flatMap {
            exec.matchingInstructionsFromInstructions(_).map {
              instrs =>
                val table = exec.emptyTable
                val startTime = System.currentTimeMillis
                for(j <- 0 until iters) {
                   exec.executeMatchingInstructions(instrs)(table)
                }
                val endTime = System.currentTimeMillis
                benchmarkPrintln(pw, "Time(" + (i + 1) + "): " + ((endTime - startTime) / 1000.0) + "s")
            }
          }
        }
      }
    } catch {
      case e: java.io.IOException =>
        consoleReader.println("io error: " + e.getMessage)
    }
  }
  
  @tailrec
  def mainLoop[T[_]](exec: Executor { type Table[U] = T[U] }, execName: String)(table: T[Int]): Unit = {
    consoleReader.setPrompt("logicalterm> ")
    val line = consoleReader.readLine()
    if(line =/= null) {
      val (table3, exitFlag) = parseCommand(line).map {
        pair =>
          pair match {
            case ("benchmark", arg) =>
              val args = arg.split("\\s+", 3)
              if(args.size >= 3) {
                val name = args(0)
                (for {
                  iters <- args(1).parseInt
                  num <- args(2).parseInt
                } yield {
                  benchmark[T](exec, name, iters, num, execName)
                }).getOrElse {
                  consoleReader.println("illegal number")
                }
              } else
                consoleReader.println("too few arguments")
              (table, ExitFlag.NoExit)
            case ("help", _)        =>
              consoleReader.println("Commands:")
              consoleReader.println(":benchmark <name> <iters> <num> measure performance")
              consoleReader.println(":help                           display this text")
              consoleReader.println(":load <file>                    load and execute the file")
              consoleReader.println(":paste                          enter the paste mode (exit from this mode is Ctrl-D)")
              consoleReader.println(":quit                           exit this program")
              consoleReader.println(":term <term>                    display the term")
              (table, ExitFlag.NoExit)
            case ("load", arg)      =>
              (if(!arg.isEmpty) {
                try {
                  val s = Source.fromFile(arg).mkString("")
                  executeWithTime[T](exec, s)(table)
                } catch {
                  case e: java.io.IOException =>
                    consoleReader.println("io error: " + e.getMessage)
                    table
                }
              } else {
                consoleReader.println("no file")
                table
              }, ExitFlag.NoExit)
            case ("paste", _)       =>
              val s = readString()
              (executeWithTime[T](exec, s)(table), ExitFlag.NoExit)
            case ("quit", _)        =>
              (table, ExitFlag.Exit)
            case ("term", arg)      =>
              Parser.parseTermString(arg).map {
                term =>
                  val res = withTime { exec.matchingTermFromTerm(term) }
                  res.map {
                    matchingTerm => consoleReader.println(matchingTerm.toString)
                  }.valueOr { err => consoleReader.println(err.toString) }
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
        case ExitFlag.NoExit => mainLoop[T](exec, execName)(table3)
      }
    }
  }
  
  val executors = Map(
      "simple" -> simpleExecutor,
      "hashSimple" -> hashSimpleExecutor,
      "rangeSimple" -> rangeSimpleExecutor,
      "range2Simple" -> range2SimpleExecutor)
  
  def main(args: Array[String]): Unit = {
    val execName = args.headOption.getOrElse("simple")
    executors.get(execName).map {
      executor =>
        mainLoop[executor.Table](executor, execName)(executor.emptyTable)
    }.getOrElse {
      Console.err.println("unknown executor")
    }
  }
}
