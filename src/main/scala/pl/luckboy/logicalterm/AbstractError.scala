package pl.luckboy.logicalterm
import scala.util.parsing.input.Position

trait AbstractError
case class Error(msg: String, pos: Position) extends AbstractError
case class FatalError(msg: String, pos: Position, stackTrace: List[java.lang.StackTraceElement] = Thread.currentThread().getStackTrace().toList) extends AbstractError