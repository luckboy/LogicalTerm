package pl.luckboy.logicalterm

sealed trait Instruction[+T]
case class Match[+T](term1: T, term2: T, matching: Matching.Value) extends Instruction[T]
case class Find[+T](term: T) extends Instruction[T]
case class Add[+T](term: T) extends Instruction[T]