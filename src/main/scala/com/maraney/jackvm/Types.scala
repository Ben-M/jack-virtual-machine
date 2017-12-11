package com.maraney.jackvm

object Types {
  sealed trait ParseResult

  sealed trait Instruction extends ParseResult
  sealed trait ArithmeticInstruction extends Instruction
  object Add extends ArithmeticInstruction
  object Sub extends ArithmeticInstruction
  object Neg extends ArithmeticInstruction
  object Eq extends ArithmeticInstruction
  object Gt extends ArithmeticInstruction
  object Lt extends ArithmeticInstruction
  object And extends ArithmeticInstruction
  object Or extends ArithmeticInstruction
  object Not extends ArithmeticInstruction

  object Malformed extends ParseResult
  object NoOp extends ParseResult

  sealed trait MemorySegment
  sealed trait WritableMemorySegment extends MemorySegment
  object Constant extends MemorySegment
  object Static extends WritableMemorySegment
  object This extends WritableMemorySegment
  object Local extends WritableMemorySegment
  object Argument extends WritableMemorySegment
  object That extends WritableMemorySegment
  object Pointer extends WritableMemorySegment
  object Temp extends WritableMemorySegment
  
  case class Push(memorySegment: MemorySegment, index: Short) extends Instruction
  case class Pop(memorySegment: WritableMemorySegment, index: Short) extends Instruction

  case class LabelName(name: String) extends AnyVal
  case class Label(label: LabelName) extends Instruction
  case class Goto(label: LabelName) extends Instruction
  case class IfGoto(label: LabelName) extends Instruction

  case class FunctionName(name: String) extends AnyVal
  case class Function(name: FunctionName, localVarCount: Short) extends Instruction
  case class Call(name: FunctionName, argCount: Short) extends Instruction
  case class Return() extends Instruction

  case class SyntaxError(line: Integer) extends Exception("", None.orNull)
}