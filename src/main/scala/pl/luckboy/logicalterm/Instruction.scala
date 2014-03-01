package pl.luckboy.logicalterm

sealed trait Instruction
case class Match(term1: Term, term2: Term, matching: Matching.Value) extends Instruction
case class Find(term: Term) extends Instruction
case class Add(term: Term) extends Instruction