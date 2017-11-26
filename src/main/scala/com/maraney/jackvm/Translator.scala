package com.maraney.jackvm

import com.maraney.jackvm.Coder.CoderState
import com.maraney.jackvm.Types.{Instruction, Malformed, SyntaxError}

import scala.util.{Failure, Success, Try}

object Translator {
  def translate(lines: List[String], fileName: String): Try[String] = {
    val parsed = lines.map(Parser.parse)
    val syntaxError = parsed.zipWithIndex.collectFirst {
      case (Malformed, line) => SyntaxError(line + 1)
    }
    if (syntaxError.isDefined) return Failure(syntaxError.get)
    val instructions = parsed.collect { case instruction: Instruction => instruction }
    val assembler = instructions.foldLeft(CoderState(0, "")) {
      (state, instruction) => Coder.code(instruction, state, fileName)
    }.assembler
    Success(assembler)
  }
}
